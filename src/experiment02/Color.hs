module Color
(
  Color(..)
, saveRender
, black
)
where

import Codec.BMP        ( packRGBA32ToBMP, writeBMP )
import Data.ByteString  ( pack )
import Data.Word        ( Word8 )


data Color = Color
  { red    :: !Double
  , green  :: !Double
  , blue   :: !Double
  }

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
