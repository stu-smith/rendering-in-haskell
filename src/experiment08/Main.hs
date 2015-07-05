module Main
(
  main
)
where

import Control.Parallel.Strategies  ( using, parList, rdeepseq )
import Numeric.FastMath             ( )
import System.Directory             ( createDirectoryIfMissing )

import Core                         ( Ray(..), Point(..), UnitVector, RayWithMedium(..)
                                    , vector, normal, normalize, translate, to, origin, cross, neg
                                    , unitX, unitY, unitZ, refractiveIndexGlass, refractiveIndexAir, (|*|), (|+|) )
import Color                        ( Color(..), saveRender, combineRender )
import Light                        ( Light(..), discLight )
import Material                     ( Material, ColorProbability(..), mkMaterial, mkEmmissive, mkRefractive )
import PhotonMap                    ( PhotonMap, generatePhotonMap )
import Rnd                          ( runRnd )
import Scene                        ( Scene, mkScene )
import Volume                       ( Volume(..), mkSphere, mkInfinitePlane )
import Render                       ( renderRay )


main :: IO ()
main = do
    putStrLn "Starting render..."
    createDirectoryIfMissing True "output"
    saveRender "output/experiment08.bmp" 640 480 $ combineRender $ map (render cam cornellBox) photonMaps
    putStrLn "Written output to output/experiment08.bmp"
  where
    cam = Ray { rayOrigin    = Point 50.0 52.0 350.0
              , rayDirection = normal 0.0 (-0.042612) (-1.0)
              }
    photonMaps = (map genPhotonMap [1..2]) `using` parList rdeepseq
    genPhotonMap seed = runRnd (seed * 1000) $ generatePhotonMap cornellBox 10000


cornellBox :: Scene
cornellBox = mkScene
    [ plane     (Point  1.0 40.8 81.6)      unitX       reddish
    , plane     (Point 99.0 40.8 81.6) (neg unitX)      blueish
    , plane     (Point 50.0 40.8  0.0)      unitZ       greyish
    , plane     (Point 50.0  0.0 81.6)      unitY       greyish
    , plane     (Point 50.0 81.6 81.6) (neg unitY)      greyish

    , sphere    (Point 27.0 16.5 47.0)  16.5            reflective
    , sphere    (Point 73.0 16.5 78.0)  16.5            refractive

    , sphere    (Point 50.0 81.4 81.6) (neg unitY) 5.0 emissiveWhite
    ]
    [ discLight (Point 50.0 81.2 81.6) (neg unitY) 10.0 (Light 2500.0 2500.0 2500.0)
    ]
  where
    emissiveWhite = mkEmmissive  (Light 2500.0 2500.0 2500.0)
    reflective    = mkMaterial   (ColorProbability 0.00 0.00 0.00) (ColorProbability 0.99 0.99 0.99)
    refractive    = mkRefractive (ColorProbability 0.10 0.10 0.10) (ColorProbability 0.89 0.89 0.89) refractiveIndexGlass
    reddish       = mkMaterial   (ColorProbability 0.50 0.25 0.25) (ColorProbability 0.00 0.00 0.00)
    blueish       = mkMaterial   (ColorProbability 0.25 0.25 0.50) (ColorProbability 0.00 0.00 0.00)
    greyish       = mkMaterial   (ColorProbability 0.50 0.50 0.50) (ColorProbability 0.00 0.00 0.00)

sphere :: Point -> Double -> Material -> Volume
sphere center radius mat =
    translate (origin `to` center) $ mkSphere radius mat

plane :: Point -> UnitVector -> Material -> Volume
plane =
    mkInfinitePlane

render :: Ray -> Scene -> PhotonMap -> Int -> Int -> Int -> Int -> Color
render (Ray camOrigin camDirection) scene photonMap !x !y !w !h =
    renderRay (RayWithMedium rr refractiveIndexAir) scene photonMap
  where !rr     = Ray { rayOrigin    = translate (d |*| focal) camOrigin
                      , rayDirection = normalize d
                      }
        !d      = (cx |*| (      dx / dw - 0.5)) |+|
                  (cy |*| (0.5 - dy / dh      )) |+|
                  camDirection
        !cx     = vector (dw * aspect / dh) 0.0 0.0
        !cy     = normalize (cx `cross` camDirection) |*| aspect
        !aspect = dh / dw / 2.0
        !focal  = 140.0
        !dw     = fromIntegral w
        !dh     = fromIntegral h
        !dx     = fromIntegral x
        !dy     = fromIntegral y
