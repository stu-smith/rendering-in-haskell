module Render
(
  renderRay
)
where

import Numeric.FastMath  ( )

import Color      ( Color )
import Core       ( Ray(..), RayWithMedium(..), RefractiveIndex
                  , (|*|), translate, calculateReflection, calculateRefraction )
import Scene      ( Intersection(..) )
import Light      ( Light, toColor, black, sumLights, red, scaled )
import Material   ( hasSpecularComponent, hasRefractiveComponent
                  , mul, specularProbabilities, refractiveProbabilities, emmissive, refractiveIndex )
import PhotonMap  ( PhotonMap, getLightToViewerAtIntersection )
import Scene      ( Scene, sceneIntersection )
import Volume     ( Volume(..) )


renderRay :: RayWithMedium -> Scene -> PhotonMap -> Color
renderRay !ray scene photonMap =
    toColor $ renderRayAsLight 4 ray scene photonMap

renderRayAsLight :: Int -> RayWithMedium -> Scene -> PhotonMap -> Light
renderRayAsLight !limit !(RayWithMedium ray riIncoming) scene photonMap =
    if limit <= 0
      then red `scaled` 1000
      else maybe black allLight maybeIntersection
  where
    maybeIntersection     = sceneIntersection scene ray
    allLight intersection = sumLights [ diffuseRender                photonMap intersection
                                      , specularRender   limit scene photonMap intersection riIncoming
                                      , emmissiveRender                        intersection
                                      , refractiveRender limit scene photonMap intersection riIncoming
                                      ]

emmissiveRender :: Intersection -> Light
emmissiveRender !intersection =
    (emmissive . volumeMaterial . volume) intersection

diffuseRender :: PhotonMap -> Intersection -> Light
diffuseRender =
    getLightToViewerAtIntersection

specularRender :: Int -> Scene -> PhotonMap -> Intersection -> RefractiveIndex -> Light
specularRender limit scene photonMap (Intersection (Ray _ incomingVec) (Volume _ nrm _ !material) _ wp) riIncoming =
    if hasSpecularComponent material
      then reflectedLight `mul` specularProbabilities material
      else black
  where
    reflectedLight   = renderRayAsLight (limit - 1) (RayWithMedium (Ray movedFromSurface reflectedRay) riIncoming) scene photonMap
    surfaceNormal    = nrm wp
    reflectedRay     = calculateReflection incomingVec surfaceNormal
    movedFromSurface = translate (reflectedRay |*| epsilon) wp
    epsilon          = 0.0001

refractiveRender :: Int -> Scene -> PhotonMap -> Intersection -> RefractiveIndex -> Light
refractiveRender limit scene photonMap (Intersection (Ray _ incomingVec) (Volume _ nrm _ !material) _ wp) riIncoming =
    if hasRefractiveComponent material
      then refractedLight `mul` refractiveProbabilities material
      else black
  where
    refractedLight        = renderRayAsLight (limit - 1) (RayWithMedium (Ray movedFromSurface refractedRay) riNew) scene photonMap             
    (refractedRay, riNew) = calculateRefraction incomingVec surfaceNormal riIncoming riMaterial
    riMaterial            = refractiveIndex material
    surfaceNormal         = nrm wp
    movedFromSurface      = translate (refractedRay |*| epsilon) wp
    epsilon               = 0.0001
