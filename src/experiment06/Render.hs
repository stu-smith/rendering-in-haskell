module Render
(
  renderRay
)
where

import Numeric.FastMath  ( )

import Color             ( Color, black )
import Core              ( Ray )
import Light             ( toColor )
import PhotonMap         ( PhotonMap, getLightToViewerAtIntersection )
import Scene             ( Scene, sceneIntersection )


renderRay :: Ray -> Scene -> PhotonMap -> Color
renderRay !ray scene photonMap =
    maybe black (toColor . getLightToViewerAtIntersection photonMap) (sceneIntersection scene ray)
