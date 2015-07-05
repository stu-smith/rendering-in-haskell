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
import Volume            ( Volume(..) )


data Scene = Scene [Volume] [PhotonLightSource]

data Intersection = Intersection
    { rayTested     :: !Ray
    , volume        :: !Volume
    , rayPosition   :: !RayPosition
    , worldPosition :: !Point
    }

mkScene :: [Volume] -> [PhotonLightSource] -> Scene
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

renderableIntersection :: Ray -> Volume -> Maybe Intersection
renderableIntersection !ray !vol =
    toIntersection <$> volumeSurfaceIntersection vol ray
  where
    toIntersection !t =
      Intersection { rayTested     = ray
                   , volume        = vol
                   , rayPosition   = t
                   , worldPosition = ray `at` t
                   }

allPhotonLightSources :: Scene -> [PhotonLightSource]
allPhotonLightSources (Scene _ !lightSources) =
    lightSources
