module Render
(
  renderRay
)
where

import Data.Maybe  ( fromMaybe )

import Color       ( Color, black )
import Core        ( Ray )
import Scene       ( Scene, Intersection(..), sceneIntersection )
import Surface     ( Surface(..) )


renderRay :: Ray -> Scene -> Color
renderRay ray scene =
    fromMaybe black maybeColor
  where maybeColor = do
          (Intersection rt (Surface _ nrm mat) rp wp) <- sceneIntersection scene ray
          return $ mat rt rp wp (nrm wp)
