module Light
(
  Light(..)
, PhotonLightSource
, colorToLight
, plus
, black
, red
, green
, blue
, colored
, scaled
, toColor
, discLight
)
where

import Core   ( Ray(..), Point, UnitVector, normalize, vector, neg, (|.|) )
import Color  ( Color(..) )
import Rnd    ( Rnd, rndDouble )


data Light = Light !Double !Double !Double

type PhotonLightSource = Rnd (Ray, Light)

colorToLight :: Color -> Light
colorToLight (Color !r !g !b) =
    Light r g b

plus :: Light -> Light -> Light
plus (Light !r1 !g1 !b1) (Light !r2 !g2 !b2) =
    Light (r1 + r2)
          (g1 + g2)
          (b1 + b2)

black :: Light
black =
    Light 0.0 0.0 0.0

red :: Light
red =
    Light 1.0 0.0 0.0

green :: Light
green =
    Light 0.0 1.0 0.0

blue :: Light
blue =
    Light 0.0 0.0 1.0

colored :: Light -> Color -> Light
colored (Light !r1 !g1 !b1) (Color !r2 !g2 !b2) =
    Light (r1 * r2) (g1 * g2) (b1 * b2)

scaled :: Light -> Double -> Light
scaled (Light !r1 !g1 !b1) !s =
    Light (r1 * s) (g1 * s) (b1 * s)

toColor :: Light -> Color
toColor (Light !r !g !b) =
    Color (clamp r) (clamp g) (clamp b)
  where
    clamp x
      | x < 0.0   = 0.0
      | x > 1.0   = 1.0
      | otherwise = x

discLight :: Point -> UnitVector -> Light -> PhotonLightSource
discLight pos normal light = do
    dx <- rndDouble 0.0 1.0
    dy <- rndDouble 0.0 1.0
    dz <- rndDouble 0.0 1.0
    let unit = normalize $ vector dx dy dz
    let dp = normal |.| unit
    let dir = if dp > 0 then unit else neg unit
    return (Ray pos dir, light)
    -- TODO this currently only emits from a point