module Main where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Text qualified as T
import Foreign.C
import Graphics.GL qualified as GL
import Graphics.UI.GLFW qualified as GLFW
import Linear
import Skia.Bindings
import Skia.Color
import Skia.Enums
import Skia.GRBackendRenderTarget qualified as GRBackendRenderTarget
import Skia.GRDirectContext qualified as GRDirectContext
import Skia.GRGlInterface qualified as GRGlInterface
import Skia.Objects
import Skia.Rect
import Skia.SKCanvas qualified as SKCanvas
import Skia.SKFont qualified as SKFont
import Skia.SKFontManager qualified as SKFontManager
import Skia.SKFontStyle qualified as SKFontStyle
import Skia.SKPaint qualified as SKPaint
import Skia.SKSurface qualified as SKSurface
import System.Exit
import System.IO

{-
Demo of setting up Skia on GLFW with an OpenGL backend.

This demo's implementation is translated from Kevin Yin
(<https://github.com/ad8e>)'s Github GIST here:
https://gist.github.com/ad8e/dd150b775ae6aa4d5cf1a092e4713add
- "instructions to use skia and glfw together. (download, installation, first
program). as of Sept 2023, Windows is broken but this is still sadly the best
starting resource for skia on Windows too."
-}

kWidth, kHeight :: Int
kWidth = 960
kHeight = 640

initSkia :: (MonadResource m) => Int -> Int -> m (GRDirectContext, SKSurface)
initSkia w h = do
  (_, interface) <- GRGlInterface.createNativeInterface
  (_, context) <- GRDirectContext.createGl interface Nothing

  let fbinfo =
        Gr_gl_framebufferinfo
          { fFBOID = 0
          , fFormat = CUInt GL.GL_RGBA8
          , fProtected = 0
          }
  (_, target) <- GRBackendRenderTarget.createGl w h 0 0 fbinfo
  isvalid <- GRBackendRenderTarget.isValid target
  unless isvalid $ error "GL backend render target is not valid"

  (_, surface) <-
    SKSurface.wrapBackendRenderTarget
      context
      target
      GRSurfaceOrigin'BottomLeft
      SKColorType'RGBA'8888
      Nothing
      Nothing
  pure (context, surface)

main :: IO ()
main = do
  -- Setup GLFW things...
  GLFW.setErrorCallback $ Just \error description -> do
    hPutStrLn stderr $ "[GLFW ERROR " <> show error <> "] " <> description

  initOk <- GLFW.init
  unless initOk exitFailure
  putStrLn "GLFW initialized"

  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  GLFW.windowHint $ GLFW.WindowHint'StencilBits (Just 0)
  GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 0)

  window <-
    GLFW.createWindow kWidth kHeight "Demo GLFW OpenGL Skia" Nothing Nothing >>= \case
      Nothing -> GLFW.terminate *> exitFailure
      Just window -> pure window

  GLFW.makeContextCurrent (Just window)
  GLFW.swapInterval 0 -- This makes the canvas refresh faster and more responsive.
  runResourceT do
    -- Setup font for drawing text.
    (_, fontmgr) <- SKFontManager.createByFontconfig
    Just (_, typeface) <- SKFontManager.matchFamilyStyle fontmgr Nothing SKFontStyle.style'Normal

    (_, font) <- SKFont.createEmpty
    SKFont.setTypeface font (Just typeface)
    SKFont.setSize font 48
    SKFont.setSkewX font (-0.5)

    -- Setup context, surface, and canvas.
    (context, surface) <- initSkia kWidth kHeight
    (_, canvas) <- SKSurface.getCanvas surface

    fix \continue -> do
      shouldClose <- liftIO $ GLFW.windowShouldClose window
      unless shouldClose do
        liftIO $ GLFW.waitEvents
        (x, y) <- liftIO $ GLFW.getCursorPos window

        -- Nest another ResourceT to isolate SKObjects related to drawing
        runResourceT do
          (_, paint) <- SKPaint.create

          -- Clear background to white
          SKPaint.setColorRGBA paint (RGBA 1 1 1 1) Nothing
          SKCanvas.drawPaint canvas paint

          -- Draw a blue rectangle for testing
          let rect =
                Rect
                  { left = realToFrac x
                  , top = realToFrac y
                  , right = 300
                  , bottom = 500
                  }
          SKPaint.setColorRGBA paint (RGBA 0 0 1 1) Nothing
          SKCanvas.drawRect canvas rect paint

          -- Draw text
          SKPaint.setColorRGBA paint (RGBA 0 0 0 1) Nothing
          SKCanvas.drawSimpleText
            canvas
            (T.pack $ "Mouse = " <> show (x, y))
            (fmap realToFrac $ V2 x y)
            font
            paint

        -- The previous draw operations/commands must be flushed before
        -- GLFW.swapBuffers, otherwise you see nothing.
        GRDirectContext.flush context
        liftIO $ GLFW.swapBuffers window

        continue

  GLFW.terminate
  putStrLn "GLFW terminated"
