module Scene
(
  Scene
, Intersection(..)
, mkScene
, sceneIntersection
, allPhotonLightSources
)
where

import Numeric.FastMath  ( )
import Data.List         ( minimumBy )
import Data.Maybe        ( mapMaybe )
import Data.Ord          ( comparing )

import Core              ( Ray(..), RayPosition, Point, at )
import Light             ( PhotonLightSource )
import Surface           ( Surface, surfaceIntersection )


data Scene = Scene [Surface] [PhotonLightSource]

data Intersection = Intersection
    { rayTested     :: !Ray
    , surface       :: !Surface
    , rayPosition   :: !RayPosition
    , worldPosition :: !Point
    }

mkScene :: [Surface] -> [PhotonLightSource] -> Scene
mkScene =
    Scene

sceneIntersection :: Scene -> Ray -> Maybe Intersection
sceneIntersection (Scene !surfaces _) !ray =
    minimumBy (comparing rayPosition) <$> maybeIntersections
  where
    allIntersections             = mapMaybe (renderableIntersection ray) surfaces
    maybeIntersections           = maybeList allIntersections
    maybeList []                 = Nothing
    maybeList xs@(_:_)           = Just xs

renderableIntersection :: Ray -> Surface -> Maybe Intersection
renderableIntersection !ray !sfc =
    toIntersection <$> surfaceIntersection sfc ray
  where
    toIntersection !t =
      Intersection { rayTested     = ray
                   , surface       = sfc
                   , rayPosition   = t
                   , worldPosition = ray `at` t
                   }

allPhotonLightSources :: Scene -> [PhotonLightSource]
allPhotonLightSources (Scene _ !lightSources) =
    lightSources
