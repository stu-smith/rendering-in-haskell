module Material
(
  Material
, flatMaterial
, diffuseMaterial
)
where

import Data.List  ( foldl' )

import Core       ( Ray, Point, RayPosition, UnitVector, normalize, to, magnitude, (|.|) )
import Color      ( Color(..) )
import Light      ( PointLight(..), Light, colorToLight, plus, black, colored, dimmed )


type Material = [PointLight] -> Ray -> RayPosition -> Point -> UnitVector -> Light

flatMaterial :: Color -> [PointLight] -> Ray -> RayPosition -> Point -> UnitVector -> Light
flatMaterial !col _ _ _ _ _ =
    colorToLight col

diffuseMaterial :: Color -> Double -> [PointLight] -> Ray -> RayPosition -> Point -> UnitVector -> Light
diffuseMaterial !col !factor !lights _ _ intersectionPosition surfaceNormal =
    foldl' plus black $ map diffuseLight lights
  where
    diffuseLight !(PointLight !lightPosition !lightColor)
        | diffuseFactor > 0 = lightColor `colored` col `dimmed` diffuseFactor
        | otherwise         = black
      where
        lightVector      = intersectionPosition `to` lightPosition
        lightDistance    = magnitude lightVector
        lightRay         = normalize lightVector
        lightAttenuation = 1.0 / lightDistance
        diffuseFactor    = factor * (surfaceNormal |.| lightRay) * lightAttenuation
