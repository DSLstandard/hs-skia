module Skia.SKParagraph.FontCollection where

import Data.Text qualified as T
import Data.Traversable
import Foreign.Marshal.Array qualified
import Skia.Bindings.Skparagraph
import Skia.Bindings.Types
import Skia.SKParagraph.Internal.Prelude
import Skia.SKString qualified as SKString

createEmpty :: (MonadResource m) => m (ReleaseKey, FontCollection)
createEmpty =
  allocateSKObjectNeverNull
    skparagraph_font_collection_new
    skparagraph_font_collection_delete

setAssetFontManager :: (MonadIO m) => FontCollection -> SKFontManager -> m ()
setAssetFontManager col fontmgr =
  liftIO $ skparagraph_font_collection_set_asset_fontmgr (ptr col) (ptr fontmgr)

setDynamicFontManager :: (MonadIO m) => FontCollection -> SKFontManager -> m ()
setDynamicFontManager col fontmgr =
  liftIO $ skparagraph_font_collection_set_dynamic_fontmgr (ptr col) (ptr fontmgr)

setTestFontManager :: (MonadIO m) => FontCollection -> SKFontManager -> m ()
setTestFontManager col fontmgr =
  liftIO $ skparagraph_font_collection_set_test_fontmgr (ptr col) (ptr fontmgr)

setDefaultFontManager ::
  (MonadIO m) =>
  FontCollection ->
  SKFontManager ->
  -- | Default family names
  [T.Text] ->
  m ()
setDefaultFontManager col fontmgr defFamilyNames = liftIO $ runResourceT do
  defFamilyNames :: [Ptr Sk_string] <- for defFamilyNames \name -> do
    (_, name) <- SKString.createFromText name
    pure (ptr name)

  liftIO $ Foreign.Marshal.Array.withArrayLen defFamilyNames \len defFamilyNames' ->
    skparagraph_font_collection_set_default_fontmgr_with_def_families
      (ptr col)
      (ptr fontmgr)
      defFamilyNames'
      (fromIntegral len)

enableFontFallback :: (MonadIO m) => FontCollection -> m ()
enableFontFallback col =
  liftIO $ skparagraph_font_collection_enable_font_fallback (ptr col)

disableFontFallback :: (MonadIO m) => FontCollection -> m ()
disableFontFallback col =
  liftIO $ skparagraph_font_collection_disable_font_fallback (ptr col)

isFontFallbackEnabled :: (MonadIO m) => FontCollection -> m Bool
isFontFallbackEnabled col =
  liftIO $ toBool <$> skparagraph_font_collection_font_fallback_enabled (ptr col)
