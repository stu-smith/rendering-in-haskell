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

import Data.List  ( foldl' )

import Core       ( Ray(..), Point, UnitVector
                  , normalize, to, magnitude, neg, (|.|), (|*|), (|+|), (|-|) )
import Color      ( Color(..) )
import Light      ( PointLight(..), Light, colorToLight, plus, black, colored, scaled )


type RenderRay = Ray -> Light

type Material = [PointLight] -> Ray -> Point -> UnitVector -> RenderRay -> Light

addMaterials :: [Material] -> [PointLight] -> Ray -> Point -> UnitVector -> RenderRay -> Light
addMaterials materials lights ray ixp surfaceNormal renderRay =
    foldl' plus black $ map (\m -> m lights ray ixp surfaceNormal renderRay) materials

scaleMaterial :: Material -> Double -> [PointLight] -> Ray -> Point -> UnitVector -> RenderRay -> Light
scaleMaterial material factor lights ray ixp surfaceNormal renderRay =
    material lights ray ixp surfaceNormal renderRay `scaled` factor

flatMaterial :: Color -> [PointLight] -> Ray -> Point -> UnitVector -> RenderRay -> Light
flatMaterial !col _ _ _ _ _ =
    colorToLight col

diffuseMaterial :: Color -> Double -> [PointLight] -> Ray -> Point -> UnitVector -> RenderRay -> Light
diffuseMaterial !col !factor !lights _ intersectionPosition surfaceNormal _ =
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

specularMaterial :: Double -> Double -> [PointLight] -> Ray -> Point -> UnitVector -> RenderRay -> Light
specularMaterial !factor !shininess !lights (Ray _ !rd) intersectionPosition surfaceNormal _ =
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

reflectiveMaterial :: Double -> [PointLight] -> Ray -> Point -> UnitVector -> RenderRay -> Light
reflectiveMaterial !factor _ (Ray _ !rd) intersectionPosition surfaceNormal renderRay =
    reflectLight `scaled` factor
  where
    c1 = - (surfaceNormal |.| rd)
    r1 = rd |+| (surfaceNormal |*| (2 * c1))
    reflectRay = Ray intersectionPosition (normalize r1)
    reflectLight = renderRay reflectRay
