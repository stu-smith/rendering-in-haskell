module Render
(
  renderRay
)
where

import Color       ( Color, black )
import Core        ( Ray )
import PhotonMap   ( PhotonMap, getColorAtIntersection )
import Scene       ( Scene, sceneIntersection )


renderRay :: Ray -> Scene -> PhotonMap -> Color
renderRay ray scene photonMap =
    maybe black (getColorAtIntersection photonMap) (sceneIntersection scene ray)
