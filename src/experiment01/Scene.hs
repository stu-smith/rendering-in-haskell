module Scene
(
  Scene
, Intersection(..)
, mkScene
, sceneIntersection
)
where

import Data.List            ( minimumBy )
import Data.Maybe           ( mapMaybe )
import Data.Ord             ( comparing )

import Core                 ( Ray, Point, at )
import Surface              ( Surface, intersection )


data Scene = Scene [Surface]

data Intersection = Intersection
    { rayTested     :: Ray
    , surface       :: Surface
    , rayPosition   :: Double
    , worldPosition :: Point
    }

mkScene :: [Surface] -> Scene
mkScene =
    Scene

sceneIntersection :: Scene -> Ray -> Maybe Intersection
sceneIntersection (Scene surfaces) ray =
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
