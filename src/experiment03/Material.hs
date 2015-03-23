module Material
(
  Material
, addMaterials
, flatMaterial
, diffuseMaterial
, specularMaterial
)
where

import Core       ( Ray(..), Point, UnitVector
                  , normalize, to, magnitude, neg, (|.|), (|*|), (|-|) )
import Color      ( Color(..) )
import Light      ( PointLightSource(..), Light, colorToLight, sumLights, black, colored, scaled )


type Material = [PointLightSource] -> Ray -> Point -> UnitVector -> Light

addMaterials :: [Material] -> [PointLightSource] -> Ray -> Point -> UnitVector -> Light
addMaterials materials lights ray ixp surfaceNormal =
    sumLights $ map (\m -> m lights ray ixp surfaceNormal) materials

flatMaterial :: Color -> [PointLightSource] -> Ray -> Point -> UnitVector -> Light
flatMaterial !col _ _ _ _ =
    colorToLight col

diffuseMaterial :: Color -> Double -> [PointLightSource] -> Ray -> Point -> UnitVector -> Light
diffuseMaterial !col !factor !lights _ intersectionPosition surfaceNormal =
    sumLights $ map diffuseLight lights
  where
    diffuseLight (PointLightSource !lightPosition !lightColor)
        | diffuseFactor > 0 = lightColor `colored` col `scaled` diffuseFactor
        | otherwise         = black
      where
        (lightDistance, lightRay) = lightDistanceAndRay intersectionPosition lightPosition
        lightAttenuation          = 1.0 / lightDistance
        diffuseFactor             = factor * (surfaceNormal |.| lightRay) * lightAttenuation

specularMaterial :: Double -> Double -> [PointLightSource] -> Ray -> Point -> UnitVector -> Light
specularMaterial !factor !shininess !lights (Ray _ !rd) intersectionPosition surfaceNormal =
    sumLights $ map specularLight lights
  where
    specularLight (PointLightSource !lightPosition !lightColor)
        | cosFactor > 0 = lightColor `scaled` factor `scaled` (cosFactor ** shininess) `scaled` lightAttenuation
        | otherwise     = black
      where
        (lightDistance, lightRay) = lightDistanceAndRay intersectionPosition lightPosition
        lightReflect              = surfaceNormal |*| ((surfaceNormal |*| 2) |.| lightRay) |-| lightRay
        cosFactor                 = lightReflect |.| neg rd
        lightAttenuation          = 1.0 / lightDistance

lightDistanceAndRay :: Point -> Point -> (Double, UnitVector)
lightDistanceAndRay intersectionPosition lightPosition
    = (lightDistance, lightRay)
  where
    lightVector   = intersectionPosition `to` lightPosition
    lightDistance = magnitude lightVector
    lightRay      = normalize lightVector
