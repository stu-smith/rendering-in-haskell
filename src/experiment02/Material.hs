module Material
(
  Material
, flatMaterial
, diffuseMaterial
)
where

import Data.List  ( foldl' )

import Core       ( Ray, Point, UnitVector, normalize, to, magnitude, (|.|) )
import Color      ( Color(..) )
import Light      ( PointLightSource(..), Light, colorToLight, plus, black, colored, scaled )


type Material = [PointLightSource] -> Ray -> Point -> UnitVector -> Light

flatMaterial :: Color -> [PointLightSource] -> Ray -> Point -> UnitVector -> Light
flatMaterial !col _ _ _ _ =
    colorToLight col

diffuseMaterial :: Color -> Double -> [PointLightSource] -> Ray -> Point -> UnitVector -> Light
diffuseMaterial !col !factor !lights _ intersectionPosition surfaceNormal =
    foldl' plus black $ map diffuseLight lights
  where
    diffuseLight (PointLightSource !lightPosition !lightColor)
        | diffuseFactor > 0 = lightColor `colored` col `scaled` diffuseFactor
        | otherwise         = black
      where
        lightVector      = intersectionPosition `to` lightPosition
        lightDistance    = magnitude lightVector
        lightRay         = normalize lightVector
        lightAttenuation = 1.0 / lightDistance
        diffuseFactor    = factor * (surfaceNormal |.| lightRay) * lightAttenuation
