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
import Skia.SkColor
import Skia.SkCanvas qualified as SkCanvas
import Skia.Skottie.Animation qualified as Animation
import Skia.Skottie.AnimationBuilder qualified as AnimationBuilder
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
  (builderKey, builder) <- AnimationBuilder.create AnimationBuilder.defaultBuilderFlags
  (_animKey, anim) <-
    AnimationBuilder.buildFromFile builder animFilePath >>= \case
      Nothing -> do
        liftIO $ putStrLn $ "[!] Error: Cannot build skottie animation from file " <> animFilePath
        liftIO $ putStrLn $ "[!] Quitting..."
        liftIO $ exitFailure
      Just anim -> do
        pure anim
  release builderKey

  -- Print animation info
  duration <- Animation.getDuration anim
  V2 width height <- Animation.getSize anim
  fps <- Animation.getFPS anim
  inPoint <- Animation.getInPoint anim
  outPoint <- Animation.getInPoint anim
  version <- Animation.getVersion anim

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

      SkCanvas.clearRGBA canvas $ RGBA 1.0 1.0 1.0 1.0

      nowTime <- liftIO $ Time.getPOSIXTime
      let t = realToFrac (nowTime - startTime) `mod'` duration
      Animation.seekFrameTime anim (realToFrac t) Nothing
      Animation.render
        anim
        canvas
        Nothing
        Animation.defaultRenderFlags

      liftIO $ flushCanvas
      liftIO $ GLFW.swapBuffers window

      loop
