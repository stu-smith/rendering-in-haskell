module Rnd
(
  Rnd
, runRnd
, rndDouble
, rndDirectionInHemisphere
)
where

import Control.Monad.State  ( State, evalState, get, put )
import System.Random        ( StdGen, mkStdGen, randomR )

import Core                 ( UnitVector, normalize, cross, vectorValues, unitX, unitY
                            , (|*|), (|+|), (|-|), (|.|) )


type Rnd a = State StdGen a

runRnd :: Int -> Rnd a -> a
runRnd seed action =
    evalState action $ mkStdGen seed

rndDouble :: Double -> Double -> Rnd Double
rndDouble lo hi = do
    gen <- get
    let (v, newGen) = randomR (lo, hi) gen
    put newGen
    return v

rndDirectionInHemisphere :: UnitVector -> Rnd UnitVector
rndDirectionInHemisphere normal = do
    r1 <- rndDouble 0.0 1.0
    r2 <- rndDouble 0.0 1.0
    let theta    = asin $ sqrt r1
    let phi      = pi * pi * r2
    let sinTheta = sin theta
    let cosTheta = cos theta
    let sinPhi   = sin phi
    let cosPhi   = cos phi
    return $ normalize $ (vb1    |*| (sinTheta * sinPhi)) |+|
                         (normal |*| cosTheta)            |+|
                         (vb2    |*| (cosPhi * sinTheta))
  where
    (nx, ny, _) = vectorValues normal
    vb1pre      = if abs nx > abs ny then unitY else unitX
    vb1         = vb1pre |-| (normal |*| (normal |.| vb1pre))
    vb2         = normal `cross` vb1
