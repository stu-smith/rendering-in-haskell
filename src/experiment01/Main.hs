module Main
(
  main
)
where

import System.Directory  ( createDirectoryIfMissing )

import Core              ( Ray(..), Point(..), Vector(..)
                         , normalize, translate, to, origin, cross
                         , (|*|), (|+|) )
import Color             ( Color(..), saveRender )
import Scene             ( Scene, mkScene )
import Surface           ( Surface(..), mkSphere, mkPlane )
import Render            ( renderRay )


main :: IO ()
main = do
    putStrLn "Starting render..."
    createDirectoryIfMissing True "output"
    saveRender "output/experiment01.png" 640 480 $ render cam cornellBox
    putStrLn "Written output to output/experiment01.png"
  where cam = Ray { rayOrigin    = Point 50.0 52.0 295.6
                  , rayDirection = normalize $ Vector 0.0 (-0.042612) (-1.0)
                  }


cornellBox :: Scene
cornellBox = mkScene
    [ plane  (Point   1.0  40.8  81.6) (Vector   1.0   0.0   0.0)  (Color 0.75 0.25 0.25)
    , plane  (Point  99.0  40.8  81.6) (Vector (-1.0)  0.0   0.0)  (Color 0.25 0.25 0.75)
    , plane  (Point  50.0  40.8   0.0) (Vector   0.0   0.0   1.0)  (Color 0.75 0.75 0.75)
    , plane  (Point  50.0   0.0  81.6) (Vector   0.0   1.0   0.0)  (Color 0.75 0.75 0.75)
    , plane  (Point  50.0  81.6  81.6) (Vector   0.0 (-1.0)  0.0)  (Color 0.75 0.75 0.75)
    , plane  (Point  50.0  40.8 170.0) (Vector   0.0   0.0 (-1.0)) (Color 0.00 0.00 0.00)

    , sphere (Point  27.0  16.5  47.0)  16.5                       (Color 0.99 0.99 0.99)
    , sphere (Point  73.0  16.5  78.0)  16.5                       (Color 0.99 0.99 0.99)

    , sphere (Point  50.0 681.33 81.6) 600.0                       (Color 1.00 1.00 1.00)
    ]


sphere :: Point -> Double -> Color -> Surface
sphere center radius color =
    translate (origin `to` center) $ mkSphere radius color

plane :: Point -> Vector -> Color -> Surface
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
        cx     = Vector (dw * aspect / dh) 0.0 0.0
        cy     = normalize (cx `cross` camDirection) |*| aspect
        aspect = dh / dw / 2.0
        focal  = 140.0
        dw     = fromIntegral w
        dh     = fromIntegral h
        dx     = fromIntegral x
        dy     = fromIntegral y
