module Rnd
(
  Rnd
, runRnd
, rndDouble
)
where

import Control.Monad.State  ( State, evalState, get, put )
import System.Random        ( StdGen, mkStdGen, randomR )


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
