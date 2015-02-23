module Render
(
  renderRay
)
where

import Data.Maybe  ( fromMaybe )

import Color       ( Color )
import Core        ( Ray )
import Light       ( Light, toColor, black )
import Scene       ( Scene, Intersection(..), sceneIntersection, pointLights )
import Surface     ( Surface(..) )


renderRay :: Ray -> Scene -> Color
renderRay ray scene =
    toColor $ renderRayRecursive scene 4 ray

renderRayRecursive :: Scene -> Int -> Ray -> Light
renderRayRecursive scene level ray
    | level <= 0 = black
    | otherwise  = fromMaybe black maybeColor
  where
    maybeColor = do
        (Intersection rt (Surface _ nrm mat) _ wp) <- sceneIntersection scene ray
        return $ mat (pointLights scene) rt wp (nrm wp) $ renderRayRecursive scene (level - 1)
