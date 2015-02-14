module Render
(
  renderRay
)
where

import Color    ( Color(..) )
import Core     ( Ray )
import Scene    ( Scene, Intersection(..), sceneIntersection )
import Surface  ( Surface(..) )


renderRay :: Ray -> Scene -> Color
renderRay ray scene =
    getColor maybeIntersection
  where
    maybeIntersection = sceneIntersection scene ray
    getColor Nothing                                   = Color 0.0 0.0 0.0
    getColor (Just (Intersection _ (Surface _ c) _ _)) = c
