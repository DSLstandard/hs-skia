module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Linear
import Skia.Color
import Skia.Enums
import Skia.SKCanvas qualified as SKCanvas
import Skia.SKColorSpace qualified as SKColorSpace
import Skia.SKFileWStream qualified as SKFileWStream
import Skia.SKImage qualified as SKImage
import Skia.SKImageEncoder qualified as SKImageEncoder
import Skia.SKImageInfo
import Skia.SKPaint qualified as SKPaint
import Skia.SKPixmap qualified as SKPixmap
import Skia.SKSurface qualified as SKSurface

main :: IO ()
main = do
  let outputPngFilePath = "raster-example-output.png"

  -- Example somewhat follows the Raster code from
  -- https://skia.org/docs/user/api/skcanvas_creation/.
  runResourceT do
    (_, colorspace) <- SKColorSpace.createSRGB
    let
      iminfo =
        SKImageInfo
          { colorspace
          , width = 800
          , height = 800
          , colorType = SKColorType'ARGB'4444
          , alphaType = SKAlphaType'Premul
          }
    (_, surf) <- SKSurface.createRaster iminfo Nothing Nothing
    (_, canvas) <- SKSurface.getCanvas surf
    SKCanvas.clearRGBA canvas (RGBA 0 1 1 1)

    (release -> deletePaint, paint) <- SKPaint.create
    SKPaint.setColor paint 0xFF0000FF
    SKCanvas.drawCircle canvas (V2 400 400) 200 paint
    deletePaint

    (_, image) <- SKSurface.makeImageSnapshot surf Nothing
    (_, fileStream) <- SKFileWStream.open outputPngFilePath

    -- Logic is derived from SkiaSharp's binding/SkiaSharp/SKImage.cs's SKImage::Encode(format, quality):
    -- https://github.com/mono/SkiaSharp/blob/2b3f5bf292e1c95dd5a8647ff3a49ebb59d4d770/binding/SkiaSharp/SKImage.cs#L363
    (_, rasterImage) <- SKImage.createRasterImage image
    (_, pixmap) <- SKPixmap.createEmpty
    ok <- SKImage.peekPixels rasterImage pixmap
    unless ok $ error "SKImage.peekPixels failed"

    let opts = SKImageEncoder.EncoderOptions'Png SKImageEncoder.defaultSKPngEncoderOptions
    ok <- SKImageEncoder.encodePixmap fileStream pixmap opts
    unless ok $ error "SKImageEncoder.encodePixmap failed"

    liftIO $ putStrLn $ "[!] Saved image to '" <> outputPngFilePath <> "'"
