module Main
(
  main
)
where

import System.Directory  ( createDirectoryIfMissing )

import Core              ( Ray(..), Point(..), UnitVector
                         , vector, normal, normalize, translate, to, origin, cross
                         , (|*|), (|+|) )
import Color             ( Color(..), saveRender )
import Light             ( PointLightSource(..), Light(..) )
import Material          ( Material, diffuseMaterial, flatMaterial, specularMaterial, addMaterials )
import Scene             ( Scene, mkScene )
import Surface           ( Surface(..), mkSphere, mkPlane )
import Render            ( renderRay )


main :: IO ()
main = do
    putStrLn "Starting render..."
    createDirectoryIfMissing True "output"
    saveRender "output/experiment03.bmp" 640 480 $ render cam cornellBox
    putStrLn "Written output to output/experiment03.bmp"
  where cam = Ray { rayOrigin    = Point 50.0 52.0 295.6
                  , rayDirection = normal 0.0 (-0.042612) (-1.0)
                  }


cornellBox :: Scene
cornellBox = mkScene
    [ plane  (Point   1.0  40.8  81.6) (normal   1.0   0.0   0.0)  reddish
    , plane  (Point  99.0  40.8  81.6) (normal (-1.0)  0.0   0.0)  blueish
    , plane  (Point  50.0  40.8   0.0) (normal   0.0   0.0   1.0)  greyish
    , plane  (Point  50.0   0.0  81.6) (normal   0.0   1.0   0.0)  greyish
    , plane  (Point  50.0  81.6  81.6) (normal   0.0 (-1.0)  0.0)  greyish
    , plane  (Point  50.0  40.8 170.0) (normal   0.0   0.0 (-1.0)) pureBlack

    , sphere (Point  27.0  16.5  47.0)  16.5                       shiny
    , sphere (Point  73.0  16.5  78.0)  16.5                       shiny

    , sphere (Point  50.0 681.33 81.6) 600.0                       emissiveWhite
    ]
    [ PointLightSource (Point 50.0 79.33 81.6) (Light 100.0 100.0 100.0)
    ]
  where
    ambient       = flatMaterial (Color 0.1 0.1 0.1)
    emissiveWhite = flatMaterial (Color 1.00 1.00 1.00)
    shiny         = addMaterials [ ambient
                                 , diffuseMaterial (Color 0.9 0.9 0.9) 0.5
                                 , specularMaterial 0.5 10.0
                                 ]
    reddish       = addMaterials [ambient, diffuseMaterial (Color 0.75 0.25 0.25) 0.5]
    blueish       = addMaterials [ambient, diffuseMaterial (Color 0.25 0.25 0.75) 0.5]
    greyish       = addMaterials [ambient, diffuseMaterial (Color 0.75 0.75 0.75) 0.5]
    pureBlack     = flatMaterial (Color 0.00 0.00 0.00)

sphere :: Point -> Double -> Material -> Surface
sphere center radius mat =
    translate (origin `to` center) $ mkSphere radius mat

plane :: Point -> UnitVector -> Material -> Surface
plane =
    mkPlane

render :: Ray -> Scene -> Int -> Int -> Int -> Int -> Color
render (Ray camOrigin camDirection) scene !x !y !w !h =
    renderRay rr scene
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
