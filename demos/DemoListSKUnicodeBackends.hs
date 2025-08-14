module Main where

import Data.Foldable
import Control.Monad.IO.Class
import Skia.SKUnicode qualified as SKUnicode
import Control.Monad.Trans.Resource

main :: IO ()
main = do
  let
    entries =
      [ ("ICU", SKUnicode.isICUSupported)
      , ("ICU4X", SKUnicode.isICU4XSupported)
      , ("Libgrapheme", SKUnicode.isLibgraphemeSupported)
      ]
    
  for_ entries \(name, isBackendSupported) -> runResourceT do
    supported <- isBackendSupported
    let msg = if supported then "Supported" else "Not supported"
    liftIO $ putStrLn $ name <> ": " <> msg