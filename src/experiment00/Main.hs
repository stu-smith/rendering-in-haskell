module Main
(
  main
)
where

import System.Directory  ( createDirectoryIfMissing )

import Color             ( Color(..), saveRender )


main :: IO ()
main = do
    putStrLn "Starting render..."
    createDirectoryIfMissing True "output"
    saveRender "output/experiment00.png" 640 480 testRenderFunction
    putStrLn "Written output to output/experiment00.png"

testRenderFunction :: Int -> Int -> Int -> Int -> Color
testRenderFunction !x !y !w !h =
     Color (fromIntegral x / fromIntegral w)
           (fromIntegral y / fromIntegral h)
           0.0
