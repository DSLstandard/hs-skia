module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Linear
import Skia.SkColor
import Skia.SkCanvas qualified as SkCanvas
import Skia.SkColorSpace qualified as SkColorSpace
import Skia.SkFileWStream qualified as SkFileWStream
import Skia.SkImage qualified as SkImage
import Skia.SkImageInfo
import Skia.SkPaint qualified as SkPaint
import Skia.SkPixmap qualified as SkPixmap
import Skia.SkPngEncoder qualified as SkPngEncoder
import Skia.SkSurface qualified as SkSurface
import Skia.SkColorType
import Skia.SkAlphaType

main :: IO ()
main = do
  let outputPngFilePath = "raster-example-output.png"

  -- Example somewhat follows the Raster code from
  -- https://skia.org/docs/user/api/skcanvas_creation/.
  runResourceT do
    (_, colorspace) <- SkColorSpace.createSRGB
    let
      iminfo =
        ImageInfo
          { colorspace = Just colorspace
          , width = 800
          , height = 800
          , colorType = SkColorType'ARGB'4444
          , alphaType = SkAlphaType'Premul
          }
    (_, surf) <- SkSurface.createRaster iminfo Nothing Nothing
    (_, canvas) <- SkSurface.getCanvas surf
    SkCanvas.clearRGBA canvas (RGBA 0 1 1 1)

    (release -> deletePaint, paint) <- SkPaint.create
    SkPaint.setColor paint 0xFF0000FF
    SkCanvas.drawCircle canvas (V2 400 400) 200 paint
    deletePaint

    (_, image) <- SkSurface.makeImageSnapshot surf Nothing
    (_, fileStream) <- SkFileWStream.open outputPngFilePath

    -- Logic is derived from SkiaSharp's binding/SkiaSharp/SkImage.cs's SkImage::Encode(format, quality):
    -- https://github.com/mono/SkiaSharp/blob/2b3f5bf292e1c95dd5a8647ff3a49ebb59d4d770/binding/SkiaSharp/SkImage.cs#L363
    (_, pixmap) <- SkPixmap.createEmpty
    ok <- SkImage.peekPixels image pixmap
    unless ok $ error "SkImage.peekPixels failed"

    ok <- SkPngEncoder.encodePixmap fileStream pixmap SkPngEncoder.defaultOptions
    unless ok $ error "SkPngEncoder.encodePixmap failed"

    liftIO $ putStrLn $ "[!] Saved image to '" <> outputPngFilePath <> "'"
