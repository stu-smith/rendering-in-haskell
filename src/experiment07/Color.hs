module Color
(
  Color(..)
, saveRender
, combineRender
, black
)
where

import Numeric.FastMath  ( )
import Codec.BMP         ( packRGBA32ToBMP, writeBMP )
import Data.ByteString   ( pack )
import Data.List         ( foldl' )
import Data.Word         ( Word8 )


data Color = Color !Double !Double !Double

saveRender :: FilePath -> Int -> Int -> (Int -> Int -> Int -> Int -> Color) -> IO ()
saveRender path width height render = do
    let bytes = concat $ do
            !y <- [height - 1, height - 2 .. 0]
            !x <- [0 .. width - 1]
            let (Color r g b) = render x y width height
            return [toByte r, toByte g, toByte b, 255]
    let rgba = pack bytes
    let bmp  = packRGBA32ToBMP width height rgba
    writeBMP path bmp

toByte :: Double -> Word8
toByte !d =
    truncate (d * 255)

black :: Color
black =
    Color 0.0 0.0 0.0

plus :: Color -> Color -> Color
plus (Color !r1 !g1 !b1) (Color !r2 !g2 !b2) =
    Color (r1 + r2)
          (g1 + g2)
          (b1 + b2)

sumColors :: [Color] -> Color
sumColors =
    foldl' plus black

scaled :: Color -> Double -> Color
scaled (Color !r1 !g1 !b1) !s =
    Color (r1 * s) (g1 * s) (b1 * s)

combineRender :: [(Int -> Int -> Int -> Int -> Color)] -> (Int -> Int -> Int -> Int -> Color)
combineRender !fns !x !y !w !h =
    summed `scaled` (1.0 / (fromIntegral $ length fns))
  where
    fx !f   = f x y w h
    !cols   = map fx fns
    !summed = sumColors cols
