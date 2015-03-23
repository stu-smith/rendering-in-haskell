module Material
(
  Material
, addMaterials
, scaleMaterial
, flatMaterial
, diffuseMaterial
, specularMaterial
, reflectiveMaterial
)
where

import Core       ( Ray(..), Point, UnitVector
                  , normalize, to, magnitude, neg, (|.|), (|*|), (|+|), (|-|) )
import Color      ( Color(..) )
import Light      ( PointLightSource(..), Light, colorToLight, sumLights, black, colored, scaled )


type RenderRay = Ray -> Light

type Material = [PointLightSource] -> Ray -> Point -> UnitVector -> RenderRay -> Light

addMaterials :: [Material] -> [PointLightSource] -> Ray -> Point -> UnitVector -> RenderRay -> Light
addMaterials materials lights ray ixp surfaceNormal renderRay =
    sumLights $ map (\m -> m lights ray ixp surfaceNormal renderRay) materials

scaleMaterial :: Material -> Double -> [PointLightSource] -> Ray -> Point -> UnitVector -> RenderRay -> Light
scaleMaterial material factor lights ray ixp surfaceNormal renderRay =
    material lights ray ixp surfaceNormal renderRay `scaled` factor

flatMaterial :: Color -> [PointLightSource] -> Ray -> Point -> UnitVector -> RenderRay -> Light
flatMaterial !col _ _ _ _ _ =
    colorToLight col

diffuseMaterial :: Color -> Double -> [PointLightSource] -> Ray -> Point -> UnitVector -> RenderRay -> Light
diffuseMaterial !col !factor !lights _ intersectionPosition surfaceNormal _ =
    sumLights $ map diffuseLight lights
  where
    diffuseLight (PointLightSource !lightPosition !lightColor)
        | diffuseFactor > 0 = lightColor `colored` col `scaled` diffuseFactor
        | otherwise         = black
      where
        (lightDistance, lightRay) = lightDistanceAndRay intersectionPosition lightPosition
        lightAttenuation          = 1.0 / lightDistance
        diffuseFactor             = factor * (surfaceNormal |.| lightRay) * lightAttenuation

specularMaterial :: Double -> Double -> [PointLightSource] -> Ray -> Point -> UnitVector -> RenderRay -> Light
specularMaterial !factor !shininess !lights (Ray _ !rd) intersectionPosition surfaceNormal _ =
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

reflectiveMaterial :: Double -> [PointLightSource] -> Ray -> Point -> UnitVector -> RenderRay -> Light
reflectiveMaterial !factor _ (Ray _ !rd) intersectionPosition surfaceNormal renderRay =
    reflectLight `scaled` factor
  where
    c1 = - (surfaceNormal |.| rd)
    r1 = rd |+| (surfaceNormal |*| (2 * c1))
    reflectRay = Ray intersectionPosition (normalize r1)
    reflectLight = renderRay reflectRay

lightDistanceAndRay :: Point -> Point -> (Double, UnitVector)
lightDistanceAndRay intersectionPosition lightPosition
    = (lightDistance, lightRay)
  where
    lightVector   = intersectionPosition `to` lightPosition
    lightDistance = magnitude lightVector
    lightRay      = normalize lightVector
