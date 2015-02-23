module Material
(
  Material
, addMaterials
, flatMaterial
, diffuseMaterial
, specularMaterial
)
where

import Data.List  ( foldl' )

import Core       ( Ray(..), Point, RayPosition, UnitVector
                  , normalize, to, magnitude, neg, (|.|), (|*|), (|-|) )
import Color      ( Color(..) )
import Light      ( PointLight(..), Light, colorToLight, plus, black, colored, scaled )


type Material = [PointLight] -> Ray -> RayPosition -> Point -> UnitVector -> Light

addMaterials :: [Material] -> [PointLight] -> Ray -> RayPosition -> Point -> UnitVector -> Light
addMaterials materials lights ray rp ixp surfaceNormal =
    foldl' plus black $ map (\m -> m lights ray rp ixp surfaceNormal) materials

flatMaterial :: Color -> [PointLight] -> Ray -> RayPosition -> Point -> UnitVector -> Light
flatMaterial !col _ _ _ _ _ =
    colorToLight col

diffuseMaterial :: Color -> Double -> [PointLight] -> Ray -> RayPosition -> Point -> UnitVector -> Light
diffuseMaterial !col !factor !lights _ _ intersectionPosition surfaceNormal =
    foldl' plus black $ map diffuseLight lights
  where
    diffuseLight (PointLight !lightPosition !lightColor)
        | diffuseFactor > 0 = lightColor `colored` col `scaled` diffuseFactor
        | otherwise         = black
      where
        lightVector      = intersectionPosition `to` lightPosition
        lightDistance    = magnitude lightVector
        lightRay         = normalize lightVector
        lightAttenuation = 1.0 / lightDistance
        diffuseFactor    = factor * (surfaceNormal |.| lightRay) * lightAttenuation

specularMaterial :: Double -> Double -> [PointLight] -> Ray -> RayPosition -> Point -> UnitVector -> Light
specularMaterial !factor !shininess !lights (Ray _ !rd) _ intersectionPosition surfaceNormal =
    foldl' plus black $ map specularLight lights
  where
    specularLight (PointLight !lightPosition !lightColor)
        | cosFactor > 0 = lightColor `scaled` factor `scaled` (cosFactor ** shininess) `scaled` lightAttenuation
        | otherwise     = black
      where
        lightVector      = intersectionPosition `to` lightPosition
        lightDistance    = magnitude lightVector
        lightRay         = normalize lightVector
        lightReflect     = surfaceNormal |*| ((surfaceNormal |*| 2) |.| lightRay) |-| lightRay
        cosFactor        = lightReflect |.| neg rd
        lightAttenuation = 1.0 / lightDistance
