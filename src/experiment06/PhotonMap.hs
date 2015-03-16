module PhotonMap
(
  PhotonMap
, PhotonSurfaceInteraction
, generatePhotonMap
, getColorAtIntersection
)
where

import Control.Monad        ( replicateM, liftM )
import Data.KdMap.Static    ( KdMap, build, inRadius )

import Color                ( Color, black )
import Core                 ( Point(..), Ray(..), UnitVector
                            , normalize, translate
                            , (|*|), (|.|), (|-|) )
import Light                ( Light, PhotonLightSource, toColor )
import Material             ( probabilityDiffuseReflection, probabilitySpecularReflection
                            , diffuseLight, specularLight )
import Rnd                  ( Rnd, rndDouble, rndDirectionInHemisphere )
import Scene                ( Scene, Intersection(..), allPhotonLightSources, sceneIntersection )
import Surface              ( Surface(..) )


data PhotonSurfaceInteraction = PhotonSurfaceInteraction UnitVector Light

type PhotonMap = KdMap Double Point PhotonSurfaceInteraction

generatePhotonMap :: Scene -> Int -> Rnd PhotonMap
generatePhotonMap scene num =
    liftM (build pointToList) $ generatePhotonSurfaceInxs scene num
  where
    pointToList (Point !x !y !z) = [x, y, z]

generatePhotonSurfaceInxs :: Scene -> Int -> Rnd [(Point, PhotonSurfaceInteraction)]
generatePhotonSurfaceInxs scene num =
    concatM $ mapM (generatePhotonSurfaceInxsForLightSource scene numPerLight) lightSources
  where
    lightSources = allPhotonLightSources scene
    numPerLight  = num `div` length lightSources

generatePhotonSurfaceInxsForLightSource :: Scene -> Int -> PhotonLightSource -> Rnd [(Point, PhotonSurfaceInteraction)]
generatePhotonSurfaceInxsForLightSource scene num lightSource =
    concatM $ replicateM num $ generateSinglePhotonSurfaceInxn scene lightSource

generateSinglePhotonSurfaceInxn :: Scene -> PhotonLightSource -> Rnd [(Point, PhotonSurfaceInteraction)]
generateSinglePhotonSurfaceInxn scene lightSource =
    lightSource >>= traceLightRay scene

traceLightRay :: Scene -> (Ray, Light) -> Rnd [(Point, PhotonSurfaceInteraction)]
traceLightRay scene incoming@(incomingRay, incomingLight) =
    case maybeIntersection of
      Nothing -> return []
      Just ix -> do
          maybeOutgoingLight <- computeOutgoingLightRay ix incoming
          let photonIntersection = toPhotonIntersection ix
          recurse <- maybe (return []) (traceLightRay scene) maybeOutgoingLight
          return (photonIntersection : recurse)
  where
    maybeIntersection = sceneIntersection scene incomingRay
    toPhotonIntersection (Intersection (Ray _ rd) _ _ pos) =
      (pos, PhotonSurfaceInteraction rd incomingLight)

computeOutgoingLightRay :: Intersection -> (Ray, Light) -> Rnd (Maybe (Ray, Light))
computeOutgoingLightRay (Intersection _ (Surface _ nrm material) _ wp) ((Ray _ incomingRay), incomingLight) = do
    prob <- rndDouble 0.0 1.0
    go prob
  where
    pd = probabilityDiffuseReflection  material
    ps = probabilitySpecularReflection material
    go prob | prob < pd      = goDiffuse
            | prob < pd + ps = goSpecular
            | otherwise      = return Nothing
    goDiffuse = do
        dr <- diffuseReflect surfaceNormal
        return $ Just ( Ray movedFromSurface dr
                      , diffuseLight  material incomingLight
                      )
    goSpecular =
        return $ Just ( Ray movedFromSurface $ specularReflect surfaceNormal incomingRay
                      , specularLight material incomingLight
                      )
    surfaceNormal = nrm wp
    movedFromSurface = translate (surfaceNormal |*| epsilon) wp
    epsilon = 0.0001

specularReflect :: UnitVector -> UnitVector -> UnitVector
specularReflect surfaceNormal incomingRay =
    normalize $ surfaceNormal |*| ((surfaceNormal |*| 2) |.| incomingRay) |-| incomingRay

diffuseReflect :: UnitVector -> Rnd UnitVector
diffuseReflect =
    rndDirectionInHemisphere

getColorAtIntersection :: PhotonMap -> Intersection -> Color
getColorAtIntersection photonMap (Intersection _ _ _ wp) =
    go nearInteractions
  where
    nearInteractions = inRadius photonMap 0.5 wp
    go [] = black
    go ((_, PhotonSurfaceInteraction _ light):_) = toColor light

concatM :: Monad m => m [[a]] -> m [a]
concatM =
    liftM concat
