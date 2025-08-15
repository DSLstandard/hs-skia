module Main where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Fixed (mod')
import Data.Text qualified as T
import Data.Time.Clock.POSIX qualified as Time
import Graphics.UI.GLFW qualified as GLFW
import Linear
import Options.Applicative qualified as Opts
import SharedUtils.MakeDemoApp (runDemoCanvasWindowApp)
import Skia.Color
import Skia.SKCanvas qualified as SKCanvas
import Skia.Skottie.Animation qualified as SkottieAnimation
import Skia.Skottie.AnimationBuilder qualified as SkottieAnimationBuilder
import System.Directory
import System.Exit

parseArgs :: IO FilePath
parseArgs = do
  Opts.execParser $
    Opts.info
      ( Opts.helper
          <*> Opts.strOption
            ( Opts.short 'f'
                <> Opts.long "lottie-file"
                <> Opts.metavar "FILE"
                <> Opts.help "Specify the Lottie animation to display"
                <> Opts.value "./assets/lottie-animations/lottie-logo.json"
                <> Opts.showDefault
            )
      )
      ( Opts.fullDesc
          <> Opts.progDesc "Demo Skottie animation viewer"
      )

main :: IO ()
main = do
  animFilePath <- parseArgs
  animFileExists <- doesFileExist animFilePath

  if animFileExists
    then do
      putStrLn $ "Loading animation from file: " <> animFilePath
      startAnimationApp animFilePath
    else do
      putStrLn $ "[!] Error: The specified animation file does not exist: " <> animFilePath
      putStrLn "[!] Quitting..."
      exitFailure

startAnimationApp :: FilePath -> IO ()
startAnimationApp animFilePath = runDemoCanvasWindowApp "DemoSkottieAnimationViewer" (V2 400 400) \window obtainCanvas -> do
  -- Load animation
  (builderKey, builder) <- SkottieAnimationBuilder.create SkottieAnimationBuilder.defaultCreateFlags
  (_animKey, anim) <-
    SkottieAnimationBuilder.buildFromFile builder animFilePath >>= \case
      Nothing -> do
        liftIO $ putStrLn $ "[!] Error: Cannot build SkottieAnimation from file " <> animFilePath
        liftIO $ putStrLn $ "[!] Quitting..."
        liftIO $ exitFailure
      Just anim -> do
        pure anim
  release builderKey

  -- Print animation info
  duration <- SkottieAnimation.getDuration anim
  V2 width height <- SkottieAnimation.getSize anim
  fps <- SkottieAnimation.getFPS anim
  inPoint <- SkottieAnimation.getInPoint anim
  outPoint <- SkottieAnimation.getInPoint anim
  version <- SkottieAnimation.getVersion anim

  liftIO $ putStrLn "Info about the loaded animation:"
  liftIO $ putStrLn $ "- Duration (seconds): " <> show duration
  liftIO $ putStrLn $ "- Dimensions: " <> show width <> "x" <> show height
  liftIO $ putStrLn $ "- Frames per second: " <> show fps
  liftIO $ putStrLn $ "- Animation in point (frame index units): " <> show inPoint
  liftIO $ putStrLn $ "- Animation out point (frame index units): " <> show outPoint
  liftIO $ putStrLn $ "- Version: " <> T.unpack version

  startTime <- liftIO $ Time.getPOSIXTime

  -- Main loop
  fix \loop -> do
    shouldClose <- liftIO $ GLFW.windowShouldClose window
    unless shouldClose do
      liftIO $ GLFW.pollEvents

      (canvas, flushCanvas) <- obtainCanvas

      SKCanvas.clearRGBA canvas $ RGBA 1.0 1.0 1.0 1.0

      nowTime <- liftIO $ Time.getPOSIXTime
      let t = realToFrac (nowTime - startTime) `mod'` duration
      SkottieAnimation.seekFrameTime anim (realToFrac t) Nothing
      SkottieAnimation.render
        anim
        canvas
        Nothing
        SkottieAnimation.defaultRenderFlags

      liftIO $ flushCanvas
      liftIO $ GLFW.swapBuffers window

      loop
