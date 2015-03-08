module Render
(
  renderRay
)
where

import Data.Maybe  ( fromMaybe )

import Color       ( Color )
import Core        ( Ray )
import Light       ( Light, toColor, black )
import Scene       ( Scene, TaggedSurface(..), Intersection(..)
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
        (Intersection rt ts@(TaggedSurface _ (Surface _ nrm mat)) _ wp) <- sceneIntersection scene Nothing ray
        let lights          = pointLightSourcesVisibleFrom scene ts wp
        let surfaceNormal   = nrm wp
        let recursiveRender = renderRayRecursive scene (level - 1)
        return $ mat lights rt wp surfaceNormal recursiveRender
