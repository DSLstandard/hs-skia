module SharedUtils.MakeDemoApp (
  runDemoCanvasWindowApp,
)
where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Acquire
import Data.Coerce
import Data.Foldable
import Data.IORef
import Data.Text qualified as T
import Foreign hiding (void)
import Foreign.C
import Graphics.GL qualified as GL
import Graphics.UI.GLFW qualified as GLFW
import Linear
import Skia.GrBackendRenderTarget qualified as GrBackendRenderTarget
import Skia.GrContextOptions qualified as GrContextOptions
import Skia.GrDirectContext qualified as GrDirectContext
import Skia.GrGLInterface qualified as GrGLInterface
import Skia.GrGLTypes
import Skia.GrTypes
import Skia.Objects
import Skia.SkColorType
import Skia.SkSurface qualified as SkSurface
import System.Exit
import System.IO

-- | See 'runDemoCanvasWindowApp' for more description.
type DemoCanvasWindowApp =
  -- | App window
  GLFW.Window ->
  -- | Function to obtain:
  --
  -- 1. SkCanvas for the current frame and Callback to flush draw commands.
  --
  -- 2. This function should be called before swapping the draw buffers of the
  -- window (e.g., using `GLFW.swapBuffers window`).
  ResourceT IO (SkCanvas, IO ()) ->
  ResourceT IO ()

{- | Creates a GLFW window with an associated canvas and an 'IO' action to flush
to underlying drawing commands, which should be called before a window update.

The input function takes these objects and shall create a GLFW loop (i.e., check
GLFW.windowShouldClose, GLFW.waitEvents, GLFW.swapBuffers, etc). When the
function ends, the window is destroyed automatically.
-}
runDemoCanvasWindowApp ::
  -- | Title of the window
  String ->
  -- | Window size
  V2 Int ->
  -- | Application entrypoint
  DemoCanvasWindowApp ->
  IO ()
runDemoCanvasWindowApp winTitle (V2 kWidth kHeight) application = do
  -- NOTE: This is almost a direct copy of demos/DemoGLFWOpenGL.hs
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
    GLFW.createWindow kWidth kHeight winTitle Nothing Nothing >>= \case
      Nothing -> GLFW.terminate *> exitFailure
      Just window -> pure window

  GLFW.makeContextCurrent (Just window)
  GLFW.swapInterval 0

  runResourceT do
    (_, interface) <- GrGLInterface.createNativeInterface
    (_, context) <- GrDirectContext.makeGL interface GrContextOptions.defaultOptions

    envRef <- liftIO $ newIORef Nothing
    let
      createEnv :: V2 Int -> ResourceT IO (GrBackendRenderTarget, SkCanvas, IO ())
      createEnv (V2 w h) = do
        let
          fbinfo =
            GrGLFramebufferInfo
              { fboId = 0
              , format = GL.GL_RGBA8
              }
        (targetKey, target) <- GrBackendRenderTarget.makeGL w h 0 0 fbinfo
        valid <- GrBackendRenderTarget.isValid target
        unless valid $ error "GL backend render target is not valid"

        (surfKey, surf) <-
          SkSurface.wrapBackendRenderTarget
            context
            target
            GrSurfaceOrigin'BottomLeft
            SkColorType'RGBA'8888
            Nothing
            Nothing
        (canvasKey, canvas) <- SkSurface.getCanvas surf

        let
          releaseEnv :: IO ()
          releaseEnv = do
            release targetKey
            release surfKey
            release canvasKey

        pure (target, canvas, releaseEnv)

      obtainCanvas :: ResourceT IO (SkCanvas, IO ())
      obtainCanvas = do
        (w, h) <- liftIO $ GLFW.getWindowSize window
        let currWindowSize = V2 w h

        env <- liftIO $ readIORef envRef
        canvas <- case env of
          Nothing -> do
            env@(_tgt, canvas, _onRelease) <- createEnv currWindowSize
            liftIO $ writeIORef envRef (Just env)
            pure canvas
          Just (tgt, canvas, releaseEnv) -> do
            w <- GrBackendRenderTarget.getWidth tgt
            h <- GrBackendRenderTarget.getHeight tgt
            let oldWindowSize = V2 w h

            if oldWindowSize == currWindowSize
              then do
                pure canvas
              else do
                liftIO releaseEnv

                context@(_tgt, canvas, _onRelease) <- createEnv currWindowSize
                liftIO $ writeIORef envRef (Just context)
                pure canvas

        let flush = GrDirectContext.flush context
        pure (canvas, flush)

    application window obtainCanvas

  GLFW.terminate
  putStrLn "GLFW terminated"
