module Light
(
  Light(..)
, PointLight(..)
)
where

import Core  ( Point )


data Light = Light !Double !Double !Double

data PointLight = PointLight !Point !Light
