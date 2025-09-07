module Main where

import Data.Foldable
import Control.Monad.IO.Class
import Skia.SkUnicode.SkUnicode qualified as SkUnicode
import Control.Monad.Trans.Resource

main :: IO ()
main = do
  let
    entries =
      [ ("ICU", SkUnicode.isICUAvailable)
      , ("ICU4X", SkUnicode.isICU4XAvailable)
      , ("Libgrapheme", SkUnicode.isLibgraphemeAvailable)
      ]
    
  for_ entries \(name, isBackendAvailable) -> runResourceT do
    available <- isBackendAvailable
    let msg = if available then "Available" else "Not available"
    liftIO $ putStrLn $ name <> ": " <> msg