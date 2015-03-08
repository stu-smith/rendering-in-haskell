module Scene
(
  Scene
, Intersection(..)
, TaggedSurface(..)
, mkScene
, sceneIntersection
, allPointLightSources
, pointLightSourcesVisibleFrom
)
where

import Control.Applicative  ( (<$>) )
import Data.List            ( delete, minimumBy )
import Data.Maybe           ( mapMaybe )
import Data.Ord             ( comparing )

import Core                 ( Ray(..), RayPosition, Point, at, normalizeWithLength, to )
import Light                ( PointLightSource(..) )
import Surface              ( Surface, intersection )


data Scene = Scene [TaggedSurface] [PointLightSource]

data TaggedSurface = TaggedSurface Int Surface

instance Eq TaggedSurface where
    (TaggedSurface t1 _) == (TaggedSurface t2 _) = t1 == t2

data Intersection = Intersection
    { rayTested     :: Ray
    , surface       :: TaggedSurface
    , rayPosition   :: RayPosition
    , worldPosition :: Point
    }

mkScene :: [Surface] -> [PointLightSource] -> Scene
mkScene surfaces =
    Scene (zipWith TaggedSurface [1..] surfaces)

sceneIntersection :: Scene -> Maybe TaggedSurface -> Ray -> Maybe Intersection
sceneIntersection (Scene surfaces _) maybeExclusion ray =
      minimumBy (comparing rayPosition) <$> maybeIntersections
  where
    allIntersections             = mapMaybe (renderableIntersection ray) (validSurfaces maybeExclusion)
    maybeIntersections           = maybeList allIntersections
    maybeList []                 = Nothing
    maybeList xs@(_:_)           = Just xs
    validSurfaces Nothing        = surfaces
    validSurfaces (Just exclude) = delete exclude surfaces

renderableIntersection :: Ray -> TaggedSurface -> Maybe Intersection
renderableIntersection ray ts@(TaggedSurface _ sfc) =
    toIntersection <$> intersection sfc ray
  where
    toIntersection t =
      Intersection { rayTested     = ray
                   , surface       = ts
                   , rayPosition   = t
                   , worldPosition = ray `at` t
                   }

allPointLightSources :: Scene -> [PointLightSource]
allPointLightSources (Scene _ !pointLightSources) =
    pointLightSources

pointLightSourcesVisibleFrom :: Scene -> TaggedSurface -> Point -> [PointLightSource]
pointLightSourcesVisibleFrom scene@(Scene _ !lights) excludeSurface !point =
    filter (isLightVisibleFromPoint scene excludeSurface point) lights

isLightVisibleFromPoint :: Scene -> TaggedSurface -> Point -> PointLightSource -> Bool
isLightVisibleFromPoint !scene !excludeSurface !point (PointLightSource !lightPosition _) =
    go maybeIntersection
  where
    (!toLight, lightOffset)                  = normalizeWithLength (point `to` lightPosition)
    !rayToLight                              = Ray point toLight
    !maybeIntersection                       = sceneIntersection scene (Just excludeSurface) rayToLight
    go Nothing                               = True
    go (Just (Intersection _ _ !ixOffset _)) = ixOffset > lightOffset
