module Render
(
  renderRay
)
where

import Data.Maybe  ( fromMaybe )

import Color       ( Color )
import Core        ( Ray )
import Light       ( toColor, black )
import Scene       ( Scene, Intersection(..), sceneIntersection, pointLights )
import Surface     ( Surface(..) )


renderRay :: Ray -> Scene -> Color
renderRay ray scene =
    toColor $ fromMaybe black maybeColor
  where
    maybeColor = do
        (Intersection rt (Surface _ nrm mat) rp wp) <- sceneIntersection scene ray
        return $ mat (pointLights scene) rt rp wp (nrm wp)
