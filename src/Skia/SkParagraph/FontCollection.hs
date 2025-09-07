module Skia.SkParagraph.FontCollection where

import Data.Text qualified as T
import Data.Traversable
import Data.Vector.Storable qualified as VS
import Language.C.Inline.Cpp qualified as C
import Skia.SkParagraph.Internal.Prelude
import Skia.SkString qualified as SkString

C.context $ C.cppCtx <> C.vecCtx <> cppSkiaObjectTypes <> cppSkParagraphObjectTypes

C.include "modules/skparagraph/include/FontCollection.h"

createEmpty :: (MonadResource m) => m (ReleaseKey, FontCollection)
createEmpty =
  allocateSkObjectNeverNull
    [C.exp| skia::textlayout::FontCollection* { (new skia::textlayout::FontCollection())} |]
    (\p -> [C.block| void { delete $(skia::textlayout::FontCollection* p); } |])

setAssetFontManager :: (MonadIO m) => FontCollection -> SkFontMgr -> m ()
setAssetFontManager (ptr -> col) (ptr -> fontmgr) = liftIO do
  [C.block|void {
    $(skia::textlayout::FontCollection* col)->setAssetFontManager(sk_ref_sp($(SkFontMgr* fontmgr)));
  }|]

setDynamicFontManager :: (MonadIO m) => FontCollection -> SkFontMgr -> m ()
setDynamicFontManager (ptr -> col) (ptr -> fontmgr) = liftIO do
  [C.block|void {
    $(skia::textlayout::FontCollection* col)->setDynamicFontManager(sk_ref_sp($(SkFontMgr* fontmgr)));
  }|]

setTestFontManager :: (MonadIO m) => FontCollection -> SkFontMgr -> m ()
setTestFontManager (ptr -> col) (ptr -> fontmgr) = liftIO do
  [C.block|void {
    $(skia::textlayout::FontCollection* col)->setTestFontManager(sk_ref_sp($(SkFontMgr* fontmgr)));
  }|]
setDefaultFontManager ::
  (MonadIO m) =>
  FontCollection ->
  SkFontMgr ->
  -- | Default family names
  [T.Text] ->
  m ()
setDefaultFontManager (ptr -> col) (ptr -> fontmgr) defFamilyNames = liftIO $ runResourceT do
  skStrings <- VS.fromList <$> for defFamilyNames \name -> do
    (_, txt) <- SkString.createFromText name
    pure (ptr txt)

  liftIO [C.block| void {
    std::vector<SkString> families($vec-len:skStrings);

    for (int i = 0; i < $vec-len:skStrings; i++) {
      auto str = $vec-ptr:(SkString** skStrings)[i];
      families.emplace_back(*str);
    }

    auto col = $(skia::textlayout::FontCollection* col);
    auto fontmgr = $(SkFontMgr* fontmgr);

    col->setDefaultFontManager(sk_ref_sp(fontmgr), families);
  }|]

enableFontFallback :: (MonadIO m) => FontCollection -> m ()
enableFontFallback (ptr -> col) = liftIO do
  [C.block|void {
    $(skia::textlayout::FontCollection* col)->enableFontFallback();
  }|]

disableFontFallback :: (MonadIO m) => FontCollection -> m ()
disableFontFallback (ptr -> col) = liftIO do
  [C.block|void {
    $(skia::textlayout::FontCollection* col)->disableFontFallback();
  }|]

isFontFallbackEnabled :: (MonadIO m) => FontCollection -> m Bool
isFontFallbackEnabled (ptr -> col) = liftIO do
  toBool <$> [C.exp|bool {
    $(skia::textlayout::FontCollection* col)->fontFallbackEnabled()
  }|]