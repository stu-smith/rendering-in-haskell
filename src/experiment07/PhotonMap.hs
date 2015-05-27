
module PhotonMap
(
  PhotonMap
, PhotonSurfaceInteraction
, count
, generatePhotonMap
, getLightToViewerAtIntersection
)
where

import Numeric.FastMath     ( )
import Control.DeepSeq      ( NFData(..), force )
import Control.Monad        ( replicateM, liftM )
import Data.KdMap.Static    ( KdMap, buildWithDist, inRadius )

import Core                 ( Point(..), Ray(..), UnitVector
                            , normalize, translate, neg, magnitude, to, (|*|), (|.|), (|-|) )
import Light                ( Light, PhotonLightSource, sumLights, scaled )
import Material             ( probabilityDiffuseReflection, probabilitySpecularReflection
                            , diffuseLight, specularLight, brdf )
import Rnd                  ( Rnd, rndDouble, rndDirectionInHemisphere )
import Scene                ( Scene, Intersection(..), allPhotonLightSources, sceneIntersection )
import Surface              ( Surface(..) )


data PhotonSurfaceInteraction = PhotonSurfaceInteraction !UnitVector !Light

instance NFData PhotonSurfaceInteraction where
    rnf (PhotonSurfaceInteraction !v !l) = rnf v `seq` rnf l `seq` ()

data PhotonMap = PhotonMap (KdMap Double Point PhotonSurfaceInteraction) !Int !Double

instance NFData PhotonMap where
    rnf (PhotonMap !k !n !s) = rnf k `seq` rnf n `seq` rnf s `seq` ()

generatePhotonMap :: Scene -> Int -> Rnd PhotonMap
generatePhotonMap scene num = do
    psis <- generatePhotonSurfaceInxs scene num
    return $ force $ PhotonMap (buildWithDist pointToList distSquared psis) (length psis) (1.0 / fromIntegral num)
  where
    pointToList (Point !x !y !z) = [x, y, z]
    distSquared (Point !x1 !y1 !z1) (Point !x2 !y2 !z2) = xd * xd + yd * yd + zd * zd
      where
        xd = x1 - x2
        yd = y1 - y2
        zd = z1 - z2

count :: PhotonMap -> Int
count (PhotonMap _ n _) = n

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
traceLightRay !scene !incoming@(!incomingRay, incomingLight) =
    case maybeIntersection of
      Nothing -> return []
      Just ix -> do
          maybeOutgoingLight <- computeOutgoingLightRay ix incoming
          let photonIntersection = toPhotonIntersection ix
          recurse <- maybe (return []) (traceLightRay scene) maybeOutgoingLight
          return (photonIntersection : recurse)
  where
    !maybeIntersection = sceneIntersection scene incomingRay
    toPhotonIntersection (Intersection (Ray _ !rd) _ _ !pos) =
      (pos, PhotonSurfaceInteraction rd incomingLight)

computeOutgoingLightRay :: Intersection -> (Ray, Light) -> Rnd (Maybe (Ray, Light))
computeOutgoingLightRay (Intersection _ (Surface _ !nrm !material) _ !wp) (Ray _ !incomingRay, !incomingLight) = do
    prob <- rndDouble 0.0 1.0
    go prob
  where
    !pd = probabilityDiffuseReflection  material
    ps = probabilitySpecularReflection material
    go prob | prob < pd      = goDiffuse
            | prob < pd + ps = goSpecular
            | otherwise      = return Nothing
    goDiffuse = do
        dr <- diffuseReflect surfaceNormal
        return $ Just ( Ray movedFromSurface dr
                      , diffuseLight material incomingLight
                      )
    goSpecular =
        return $ Just ( Ray movedFromSurface $ specularReflect surfaceNormal incomingRay
                      , specularLight material incomingLight
                      )
    !surfaceNormal = nrm wp
    !movedFromSurface = translate (surfaceNormal |*| epsilon) wp
    !epsilon = 0.0001

specularReflect :: UnitVector -> UnitVector -> UnitVector
specularReflect !surfaceNormal !incomingRay =
    normalize $ surfaceNormal |*| ((surfaceNormal |*| 2) |.| incomingRay) |-| incomingRay

diffuseReflect :: UnitVector -> Rnd UnitVector
diffuseReflect =
    rndDirectionInHemisphere

getLightToViewerAtIntersection :: PhotonMap -> Intersection -> Light
getLightToViewerAtIntersection (PhotonMap !kdmap _ !scale) (Intersection (Ray _ !outgoingVector) (Surface _ !nrm !material) _ !wp) =
    (sumLights $ map attenuateByDistance nearInteractions) `scaled` scale
  where
    attenuateByDistance (!pp, !psi) =
      brdfForInteraction psi `scaled` coneFilter pp wp maxDistance
    brdfForInteraction (PhotonSurfaceInteraction !incomingVector !incomingLight) =
      surfaceBrdf incomingLight (neg incomingVector) (neg outgoingVector) surfaceNormal wp
    !surfaceNormal    = nrm wp
    !surfaceBrdf      = brdf material
    !nearInteractions = inRadius kdmap maxDistance wp
    !maxDistance      = 5.0

concatM :: Monad m => m [[a]] -> m [a]
concatM =
    liftM concat

coneFilter :: Point -> Point -> Double -> Double
coneFilter !pp !wp !maxDistance =
    (1.0 - distance / (2.0 * maxDistance)) / maxDistance
  where
    !distance = magnitude (pp `to` wp)

-- gaussianFilter :: Point -> Point -> Double -> Double
-- gaussianFilter !pp !wp !maxDistance =
--     a * (1.0 - (1.0 - exp (mb * px)) / dv)
--   where
--     !a   = 0.918
--     !mb  = -1.953
--     !dv = 1.0 - exp mb
--     !ds = magnitudeSquared (pp `to` wp)
--     !px = ds / (2.0 * maxDistance * maxDistance)
