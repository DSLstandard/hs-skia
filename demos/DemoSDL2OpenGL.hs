module Main where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Text qualified as T
import Foreign hiding (void)
import Graphics.GL qualified as GL
import Linear
import SDL qualified
import SDL.Raw qualified
import Skia.SkColor
import Skia.GrBackendRenderTarget qualified as GrBackendRenderTarget
import Skia.GrContextOptions qualified as GrContextOptions
import Skia.GrDirectContext qualified as GrDirectContext
import Skia.GrGLInterface qualified as GrGLInterface
import Skia.GrGLTypes
import Skia.GrTypes
import Skia.Objects
import Skia.SkRect
import Skia.SkCanvas qualified as SkCanvas
import Skia.SkColorType
import Skia.SkFont qualified as SkFont
import Skia.SkFontMgr qualified as SkFontMgr
import Skia.SkFontStyle qualified as SkFontStyle
import Skia.SkPaint qualified as SkPaint
import Skia.SkSurface qualified as SkSurface

{-
Demo of setting up Skia on SDL2 with an OpenGL backend.

This demo's implementation is translated and adapted from Kevin Yin
(<https://github.com/ad8e>)'s Github GIST here:
https://gist.github.com/ad8e/dd150b775ae6aa4d5cf1a092e4713add
- "instructions to use skia and glfw together. (download, installation, first
program). as of Sept 2023, Windows is broken but this is still sadly the best
starting resource for skia on Windows too."

Another reference is https://skia.org/docs/user/api/skcanvas_creation/.
-}

kWidth, kHeight :: Int
kWidth = 960
kHeight = 640

initSkia :: (MonadResource m) => Int -> Int -> m (GrDirectContext, SkSurface)
initSkia w h = do
  (_, interface) <- GrGLInterface.createNativeInterface
  (_, context) <- GrDirectContext.makeGL interface GrContextOptions.defaultOptions

  let
    fbinfo =
      GrGLFramebufferInfo
        { fboId = 0
        , format = GL.GL_RGBA8
        }
  (_, target) <- GrBackendRenderTarget.makeGL w h 0 0 fbinfo
  isvalid <- GrBackendRenderTarget.isValid target
  unless isvalid $ error "GL backend render target is not valid"

  (_, surface) <-
    SkSurface.wrapBackendRenderTarget
      context
      target
      GrSurfaceOrigin'BottomLeft
      SkColorType'RGBA'8888
      Nothing
      Nothing
  pure (context, surface)

getSdlMousePos :: (MonadIO m) => m (V2 Int)
getSdlMousePos = liftIO do
  alloca \x' -> alloca \y' -> do
    _ <- SDL.Raw.getMouseState x' y'
    x <- peek x'
    y <- peek y'
    pure $ (fromIntegral <$> V2 x y)

main :: IO ()
main = do
  -- Setup SDL2 things...
  SDL.initializeAll
  let
    winGraphicsConf =
      SDL.OpenGLContext
        SDL.defaultOpenGL
          { SDL.glProfile = SDL.Compatibility SDL.Normal 3 2
          , SDL.glStencilPrecision = 0
          , SDL.glMultisampleSamples = 0
          }
    winConf =
      SDL.defaultWindow
        { SDL.windowGraphicsContext = winGraphicsConf
        , SDL.windowResizable = True
        , SDL.windowInitialSize = V2 (fromIntegral kWidth) (fromIntegral kHeight)
        }
  window <- SDL.createWindow "Demo SDL2 OpenGL Skia" winConf

  glctx <- SDL.glCreateContext window
  SDL.glMakeCurrent window glctx
  void $ SDL.Raw.glSetSwapInterval 0 -- This makes the canvas refresh faster and more responsive.
  runResourceT do
    -- Setup font for drawing text.
    (_, fontmgr) <- SkFontMgr.createByFontconfig
    Just (_, typeface) <- SkFontMgr.matchFamilyStyle fontmgr Nothing SkFontStyle.style'Normal

    (_, font) <- SkFont.createDefault
    SkFont.setTypeface font (Just typeface)
    SkFont.setSize font 48
    SkFont.setSkewX font (-0.5)

    -- Setup context, surface, and canvas.
    (context, surface) <- initSkia kWidth kHeight
    (_, canvas) <- SkSurface.getCanvas surface

    let
      isEventQuit :: SDL.Event -> Bool
      isEventQuit event =
        case SDL.eventPayload event of
          SDL.WindowClosedEvent _ -> True
          _ -> False

    fix \continue -> do
      events <- SDL.pollEvents

      let shouldQuit = any isEventQuit events

      unless shouldQuit do
        V2 x y <- getSdlMousePos

        -- Nest another ResourceT to isolate SkObjects related to
        -- drawing
        runResourceT do
          (_, paint) <- SkPaint.create
          SkPaint.setColorRGBA paint (RGBA 1 1 1 1) Nothing
          SkCanvas.drawPaint canvas paint

          -- Draw a blue rectangle for testing
          let rect =
                Rect
                  { left = realToFrac x
                  , top = realToFrac y
                  , right = 300
                  , bottom = 500
                  }
          SkPaint.setColorRGBA paint (RGBA 0 0 1 1) Nothing
          SkCanvas.drawRect canvas rect paint

          -- Draw text
          SkPaint.setColorRGBA paint (RGBA 0 0 0 1) Nothing
          SkCanvas.drawSimpleText
            canvas
            (T.pack $ "Mouse = " <> show (x, y))
            (realToFrac <$> V2 x y)
            font
            paint

        -- The previous draw operations/commands must be flushed before
        -- SDL.glSwapWindow, otherwise you see nothing.
        GrDirectContext.flush context
        SDL.glSwapWindow window

        continue

  SDL.quit
  putStrLn "SDL2 terminated"
