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
import Skia.Bindings (Gr_gl_framebufferinfo (..))
import Skia.Enums
import Skia.GRBackendRenderTarget qualified as GRBackendRenderTarget
import Skia.GRDirectContext qualified as GRDirectContext
import Skia.GRGlInterface qualified as GRGlInterface
import Skia.Objects
import Skia.SKSurface qualified as SKSurface
import System.Exit
import System.IO

-- | See 'runDemoCanvasWindowApp' for more description.
type DemoCanvasWindowApp =
  -- | App window
  GLFW.Window ->
  -- | Function to obtain:
  --
  -- 1. SKCanvas for the current frame and Callback to flush draw commands.
  --
  -- 2. This function should be called before swapping the draw buffers of the
  -- window (e.g., using `GLFW.swapBuffers window`).
  ResourceT IO (SKCanvas, IO ()) ->
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
    (_, interface) <- GRGlInterface.createNativeInterface
    (_, context) <- GRDirectContext.createGl interface Nothing

    envRef <- liftIO $ newIORef Nothing
    let
      createEnv :: V2 Int -> ResourceT IO (GRBackendRenderTarget, SKCanvas, IO ())
      createEnv (V2 w h) = do
        let fbinfo = Gr_gl_framebufferinfo{fFBOID = 0, fFormat = CUInt GL.GL_RGBA8, fProtected = 0}
        (targetKey, target) <- GRBackendRenderTarget.createGl w h 0 0 fbinfo
        valid <- GRBackendRenderTarget.isValid target
        unless valid $ error "GL backend render target is not valid"

        (surfKey, surf) <-
          SKSurface.wrapBackendRenderTarget
            context
            target
            GRSurfaceOrigin'BottomLeft
            SKColorType'RGBA'8888
            Nothing
            Nothing
        (canvasKey, canvas) <- SKSurface.getCanvas surf

        let
          releaseEnv :: IO ()
          releaseEnv = do
            release targetKey
            release surfKey
            release canvasKey

        pure (target, canvas, releaseEnv)

      obtainCanvas :: ResourceT IO (SKCanvas, IO ())
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
            w <- GRBackendRenderTarget.getWidth tgt
            h <- GRBackendRenderTarget.getHeight tgt
            let oldWindowSize = V2 w h

            if oldWindowSize == currWindowSize
              then do
                pure canvas
              else do
                liftIO releaseEnv

                context@(_tgt, canvas, _onRelease) <- createEnv currWindowSize
                liftIO $ writeIORef envRef (Just context)
                pure canvas

        let flush = GRDirectContext.flush context
        pure (canvas, flush)

    application window obtainCanvas

  GLFW.terminate
  putStrLn "GLFW terminated"
