module Render
(
  renderRay
)
where

import Data.Maybe  ( fromMaybe )

import Color       ( Color )
import Core        ( Ray, (|*|), translate )
import Light       ( Light, toColor, black )
import Scene       ( Scene, Intersection(..)
                   , sceneIntersection, pointLightSourcesVisibleFrom )
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
        let surfaceNormal    = nrm wp
        let movedFromSurface = translate (surfaceNormal |*| epsilon) wp
        let lights           = pointLightSourcesVisibleFrom scene movedFromSurface
        let recursiveRender  = renderRayRecursive scene (level - 1)
        return $ mat lights rt wp surfaceNormal recursiveRender
    epsilon = 0.0001

