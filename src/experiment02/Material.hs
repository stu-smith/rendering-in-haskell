module Material
(
  Material
, flatMaterial
)
where

import Core   ( Ray, Point, RayPosition, UnitVector )
import Color  ( Color )


type Material = Ray -> RayPosition -> Point -> UnitVector -> Color

flatMaterial :: Color -> Ray -> RayPosition -> Point -> UnitVector -> Color
flatMaterial col _ _ _ _ =
    col
