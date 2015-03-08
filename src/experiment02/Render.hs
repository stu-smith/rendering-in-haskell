module Render
(
  renderRay
)
where

import Data.Maybe  ( fromMaybe )

import Color       ( Color )
import Core        ( Ray )
import Light       ( toColor, black )
import Scene       ( Scene, Intersection(..), sceneIntersection, pointLightSources )
import Surface     ( Surface(..) )


renderRay :: Ray -> Scene -> Color
renderRay ray scene =
    toColor $ fromMaybe black maybeColor
  where
    maybeColor = do
        (Intersection rt (Surface _ nrm mat) _ wp) <- sceneIntersection scene ray
        return $ mat (pointLightSources scene) rt wp (nrm wp)
