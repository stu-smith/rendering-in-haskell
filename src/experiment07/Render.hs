module Render
(
  renderRay
)
where

import Numeric.FastMath  ( )

import Color      ( Color )
import Core       ( Ray(..), (|*|), (|+|), (|.|), translate, normalize )
import Scene      ( Intersection(..) )
import Light      ( Light, toColor, black, sumLights )
import Material   ( hasSpecularComponent, mul, specularProbabilities )
import PhotonMap  ( PhotonMap, getLightToViewerAtIntersection )
import Scene      ( Scene, sceneIntersection )
import Surface    ( Surface(..) )


renderRay :: Ray -> Scene -> PhotonMap -> Color
renderRay !ray scene photonMap =
    toColor $ renderRayAsLight ray scene photonMap

renderRayAsLight :: Ray -> Scene -> PhotonMap -> Light
renderRayAsLight !ray scene photonMap =
    maybe black allLight maybeIntersection
  where
    maybeIntersection     = sceneIntersection scene ray
    allLight intersection = sumLights [ diffuseRender        photonMap intersection
                                      , specularRender scene photonMap intersection
                                      ]

diffuseRender :: PhotonMap -> Intersection -> Light
diffuseRender =
    getLightToViewerAtIntersection

specularRender :: Scene -> PhotonMap -> Intersection -> Light
specularRender scene photonMap (Intersection (Ray _ incomingVec) (Surface _ nrm material) _ wp) =
    if hasSpecularComponent material
      then reflectedLight `mul` specularProbabilities material
      else black
  where
    reflectedLight   = renderRayAsLight (Ray movedFromSurface reflectedRay) scene photonMap
    surfaceNormal    = nrm wp
    c1               = - (surfaceNormal |.| incomingVec)
    reflectedRay     = normalize (incomingVec |+| (surfaceNormal |*| (2 * c1)))
    movedFromSurface = translate (reflectedRay |*| epsilon) wp
    epsilon          = 0.0001
