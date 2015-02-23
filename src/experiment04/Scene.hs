module Scene
(
  Scene
, Intersection(..)
, mkScene
, sceneIntersection
, pointLights
)
where

import Control.Applicative  ( (<$>) )
import Data.List            ( minimumBy )
import Data.Maybe           ( mapMaybe )
import Data.Ord             ( comparing )

import Core                 ( Ray, RayPosition, Point, at )
import Light                ( PointLight )
import Surface              ( Surface, intersection )


data Scene = Scene [Surface] [PointLight]

data Intersection = Intersection
    { rayTested     :: Ray
    , surface       :: Surface
    , rayPosition   :: RayPosition
    , worldPosition :: Point
    }

mkScene :: [Surface] -> [PointLight] -> Scene
mkScene =
    Scene

sceneIntersection :: Scene -> Ray -> Maybe Intersection
sceneIntersection (Scene surfaces _) ray =
      minimumBy (comparing rayPosition) <$> maybeIntersections
  where
    allIntersections   = mapMaybe (renderableIntersection ray) surfaces
    maybeIntersections = maybeList allIntersections
    maybeList []       = Nothing
    maybeList xs@(_:_) = Just xs

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

pointLights :: Scene -> [PointLight]
pointLights (Scene _ !pls) =
    pls
