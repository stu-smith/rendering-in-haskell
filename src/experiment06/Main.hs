module Main
(
  main
)
where

import Numeric.FastMath  ( )
import Control.DeepSeq   ( deepseq )
import System.Directory  ( createDirectoryIfMissing )

import Core              ( Ray(..), Point(..), UnitVector
                         , vector, normal, normalize, translate, to, origin, cross, neg
                         , unitX, unitY, unitZ, (|*|), (|+|) )
import Color             ( Color(..), saveRender )
import Light             ( Light(..), discLight )
import Material          ( Material, ColorProbability(..), mkMaterial )
import PhotonMap         ( PhotonMap, generatePhotonMap, count )
import Rnd               ( runRnd )
import Scene             ( Scene, mkScene )
import Surface           ( Surface(..), mkSphere, mkPlane, mkDisc )
import Render            ( renderRay )


main :: IO ()
main = do
    putStrLn "Starting render..."
    createDirectoryIfMissing True "output"
    photonMap `deepseq` putStrLn ("Photon map done with " ++ show (count photonMap) ++ " interactions.")
    saveRender "output/experiment06.bmp" 640 480 $ render cam cornellBox photonMap
    putStrLn "Written output to output/experiment06.bmp"
  where
    cam = Ray { rayOrigin    = Point 50.0 52.0 295.6
              , rayDirection = normal 0.0 (-0.042612) (-1.0)
              }
    photonMap = runRnd 1 $ generatePhotonMap cornellBox 200000



cornellBox :: Scene
cornellBox = mkScene
    [ plane  (Point   1.0  40.8  81.6)      unitX  reddish
    , plane  (Point  99.0  40.8  81.6) (neg unitX) blueish
    , plane  (Point  50.0  40.8   0.0)      unitZ  greyish
    , plane  (Point  50.0   0.0  81.6)      unitY  greyish
    , plane  (Point  50.0  81.6  81.6) (neg unitY) greyish
    , plane  (Point  50.0  40.8 170.0) (neg unitZ) pureBlack

    , sphere (Point  27.0  16.5  47.0)  16.5                       reflective
    , sphere (Point  73.0  16.5  78.0)  16.5                       bitReflective

    , disc   (Point  50.0   81.0 81.6) (neg unitY) 5.0 emissiveWhite
    ]
    [ discLight (Point 50.0 79.33 81.6) (neg unitY) 10.0 (Light 2500.0 2500.0 2500.0)
    ]
  where
    emissiveWhite = mkMaterial (ColorProbability 0.99 0.99 0.99) (ColorProbability 0.00 0.00 0.00)
    reflective    = mkMaterial (ColorProbability 0.00 0.00 0.00) (ColorProbability 0.90 0.90 0.90)
    bitReflective = mkMaterial (ColorProbability 0.10 0.10 0.10) (ColorProbability 0.40 0.40 0.40)
    reddish       = mkMaterial (ColorProbability 0.50 0.25 0.25) (ColorProbability 0.00 0.00 0.00)
    blueish       = mkMaterial (ColorProbability 0.25 0.25 0.50) (ColorProbability 0.00 0.00 0.00)
    greyish       = mkMaterial (ColorProbability 0.50 0.50 0.50) (ColorProbability 0.00 0.00 0.00)
    pureBlack     = mkMaterial (ColorProbability 0.00 0.00 0.00) (ColorProbability 0.00 0.00 0.00)

sphere :: Point -> Double -> Material -> Surface
sphere center radius mat =
    translate (origin `to` center) $ mkSphere radius mat

plane :: Point -> UnitVector -> Material -> Surface
plane =
    mkPlane

disc :: Point -> UnitVector -> Double -> Material -> Surface
disc =
    mkDisc

render :: Ray -> Scene -> PhotonMap -> Int -> Int -> Int -> Int -> Color
render (Ray camOrigin camDirection) scene photonMap !x !y !w !h =
    renderRay rr scene photonMap
  where rr     = Ray { rayOrigin    = translate (d |*| focal) camOrigin
                     , rayDirection = normalize d
                     }
        d      = (cx |*| (      dx / dw - 0.5)) |+|
                 (cy |*| (0.5 - dy / dh      )) |+|
                 camDirection
        cx     = vector (dw * aspect / dh) 0.0 0.0
        cy     = normalize (cx `cross` camDirection) |*| aspect
        aspect = dh / dw / 2.0
        focal  = 140.0
        dw     = fromIntegral w
        dh     = fromIntegral h
        dx     = fromIntegral x
        dy     = fromIntegral y
