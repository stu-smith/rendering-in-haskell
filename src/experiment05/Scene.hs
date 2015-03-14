module Scene
(
  Scene
, Intersection(..)
, mkScene
, sceneIntersection
, allPointLightSources
, pointLightSourcesVisibleFrom
)
where

import Control.Applicative  ( (<$>) )
import Data.List            ( minimumBy )
import Data.Maybe           ( mapMaybe )
import Data.Ord             ( comparing )

import Core                 ( Ray(..), RayPosition, Point, at, normalizeWithLength, to )
import Light                ( PointLightSource(..) )
import Surface              ( Surface, intersection )


data Scene = Scene [Surface] [PointLightSource]

data Intersection = Intersection
    { rayTested     :: Ray
    , surface       :: Surface
    , rayPosition   :: RayPosition
    , worldPosition :: Point
    }

mkScene :: [Surface] -> [PointLightSource] -> Scene
mkScene =
    Scene

sceneIntersection :: Scene -> Ray -> Maybe Intersection
sceneIntersection (Scene surfaces _) ray =
      minimumBy (comparing rayPosition) <$> maybeIntersections
  where
    allIntersections             = mapMaybe (renderableIntersection ray) surfaces
    maybeIntersections           = maybeList allIntersections
    maybeList []                 = Nothing
    maybeList xs@(_:_)           = Just xs

renderableIntersection :: Ray -> Surface -> Maybe Intersection
renderableIntersection ray sfc =
    toIntersection <$> intersection sfc ray
  where
    toIntersection t =
      Intersection { rayTested     = ray
                   , surface       = sfc
                   , rayPosition   = t
                   , worldPosition = ray `at` t
                   }

allPointLightSources :: Scene -> [PointLightSource]
allPointLightSources (Scene _ !pointLightSources) =
    pointLightSources

pointLightSourcesVisibleFrom :: Scene -> Point -> [PointLightSource]
pointLightSourcesVisibleFrom scene@(Scene _ !lights) !point =
    filter (isLightVisibleFromPoint scene point) lights

isLightVisibleFromPoint :: Scene -> Point -> PointLightSource -> Bool
isLightVisibleFromPoint !scene !point (PointLightSource !lightPosition _) =
    go maybeIntersection
  where
    (!toLight, lightOffset)                  = normalizeWithLength (point `to` lightPosition)
    !rayToLight                              = Ray point toLight
    !maybeIntersection                       = sceneIntersection scene rayToLight
    go Nothing                               = True
    go (Just (Intersection _ _ !ixOffset _)) = ixOffset > lightOffset
