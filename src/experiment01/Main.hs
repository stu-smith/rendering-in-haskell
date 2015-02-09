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
import Scene             ( Scene, Intersection(..), mkScene, sceneIntersection )
import Surface           ( Surface(..), mkSphere )


main :: IO ()
main = do
    putStrLn "Starting render..."
    createDirectoryIfMissing True "output"
    saveRender "output/experiment01.png" 640 480 $ render cam cornellBox
    putStrLn "Written output to output/experiment01.png"
  where cam = Ray { rayOrigin    = Point 50.0 52.0 295.6
                  , rayDirection = normalize $ Vector (0.0) (-0.042612) (-1.0)
                  }


cornellBox :: Scene
cornellBox = mkScene
    [ sphere 1e5  (Point   (1e5+1)       40.8      81.6)  (Color 0.75 0.25 0.25)
    , sphere 1e5  (Point (-1e5+99)       40.8      81.6)  (Color 0.25 0.25 0.75)
    , sphere 1e5  (Point       50        40.8       1e5)  (Color 0.75 0.75 0.75)
    , sphere 1e5  (Point       50        40.8 (-1e5+170)) (Color 0.00 0.00 0.00)
    , sphere 1e5  (Point       50         1e5      81.6)  (Color 0.75 0.75 0.75)
    , sphere 1e5  (Point       50  (-1e5+81.6)     81.6)  (Color 0.75 0.75 0.75)
    , sphere 16.5 (Point       27        16.5      47.0)  (Color 0.99 0.99 0.99)
    , sphere 16.5 (Point       73        16.5      78.0)  (Color 0.99 0.99 0.99)
    , sphere 600  (Point       50      681.33      81.6)  (Color 1.00 1.00 1.00)
    ]


sphere :: Double -> Point -> Color -> Surface
sphere radius center color =
    translate (origin `to` center) $ mkSphere radius color

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


renderRay :: Ray -> Scene -> Color
renderRay ray scene =
    getColor maybeIntersection
  where
    maybeIntersection = sceneIntersection scene ray
    getColor Nothing  = Color 0.0 0.0 0.0
    getColor (Just (Intersection _ (Surface _ _ c) _ _)) = c
