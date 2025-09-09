module Skia.SkParagraph.StrutStyle where

import Data.Coerce
import Data.Text qualified as T
import Data.Traversable
import Skia.SkFontStyle
import Skia.SkParagraph.Internal.Prelude
import Skia.SkString as SkString
import Language.C.Inline.Cpp qualified as C
import Data.Vector.Storable qualified as VS

C.context $ C.cppCtx <> C.vecCtx <> cppSkiaObjectTypes <> cppSkParagraphObjectTypes

C.include "modules/skparagraph/include/ParagraphStyle.h"

-- | Creates a new empty 'StrutStyle' object.
createEmpty :: (MonadResource m) => m (ReleaseKey, StrutStyle)
createEmpty =
  allocateSkObjectNeverNull
    [C.exp| skia::textlayout::StrutStyle* { new skia::textlayout::StrutStyle() } |]
    (\p -> [C.block| void { delete $(skia::textlayout::StrutStyle* p); } |])

-- | Sets the font families for the strut style.
setFontFamilies :: (MonadIO m) => StrutStyle -> [T.Text] -> m ()
setFontFamilies (ptr -> strut) families = liftIO $ runResourceT do
  -- Allocate Sk_string objects for each family name
  families' :: VS.Vector (Ptr C'SkString) <- VS.fromList <$> for families \family -> do
    (_, familyPtr) <- SkString.createFromText family
    pure (ptr familyPtr)

  liftIO $
    [C.block| void {
      std::vector<SkString> families($vec-len:families');

      for (int i = 0; i < $vec-len:families'; i++) {
        auto str = $vec-ptr:(SkString** families')[i];
        families.emplace_back(*str);
      }

      $(skia::textlayout::StrutStyle* strut)->setFontFamilies(families);
    }|]

-- | Sets the font style for the strut style.
setFontStyle :: (MonadIO m) => StrutStyle -> FontStyle -> m ()
setFontStyle (ptr -> strut) style = evalManaged do
  fontStyle <- marshalSkFontStyle style
  liftIO $ [C.block| void {
    $(skia::textlayout::StrutStyle* strut)->setFontStyle(*$(SkFontStyle* fontStyle));
  }|]

-- | Gets the font style for the strut style.
getFontStyle :: (MonadIO m) => StrutStyle -> m FontStyle
getFontStyle (ptr -> strut) = evalManaged do
  fs' <- allocaSkFontStyle
  liftIO [C.block| void {
    *$(SkFontStyle* fs') = $(skia::textlayout::StrutStyle* strut)->getFontStyle();
  }|]
  peekSkFontStyle fs'

-- | Sets the font size for the strut style.
setFontSize :: (MonadIO m) => StrutStyle -> Float -> m ()
setFontSize (ptr -> strut) (coerce -> size) = liftIO $
  [C.block| void {
    $(skia::textlayout::StrutStyle* strut)->setFontSize($(float size));
  }|]

-- | Gets the font size for the strut style.
getFontSize :: (MonadIO m) => StrutStyle -> m Float
getFontSize (ptr -> strut) = liftIO $ coerce <$>
  [C.block| float {
    return $(skia::textlayout::StrutStyle* strut)->getFontSize();
  }|]

-- | Sets the height for the strut style.
setHeight :: (MonadIO m) => StrutStyle -> Float -> m ()
setHeight (ptr -> strut) (coerce -> height) = liftIO $
  [C.block| void {
    $(skia::textlayout::StrutStyle* strut)->setHeight($(float height));
  }|]

-- | Gets the height for the strut style.
getHeight :: (MonadIO m) => StrutStyle -> m Float
getHeight (ptr -> strut) = liftIO $ coerce <$>
  [C.block| float {
    return $(skia::textlayout::StrutStyle* strut)->getHeight();
  }|]

-- | Sets the leading for the strut style.
setLeading :: (MonadIO m) => StrutStyle -> Float -> m ()
setLeading (ptr -> strut) (coerce -> leading) = liftIO $
  [C.block| void {
    $(skia::textlayout::StrutStyle* strut)->setLeading($(float leading));
  }|]

-- | Gets the leading for the strut style.
getLeading :: (MonadIO m) => StrutStyle -> m Float
getLeading (ptr -> strut) = liftIO $ coerce <$>
  [C.block| float {
    return $(skia::textlayout::StrutStyle* strut)->getLeading();
  }|]

-- | Enables or disables the strut.
setStrutEnabled :: (MonadIO m) => StrutStyle -> Bool -> m ()
setStrutEnabled (ptr -> strut) (fromBool -> enabled) = liftIO $
  [C.block| void {
    $(skia::textlayout::StrutStyle* strut)->setStrutEnabled($(bool enabled));
  }|]

-- | Checks if strut is enabled.
getStrutEnabled :: (MonadIO m) => StrutStyle -> m Bool
getStrutEnabled (ptr -> strut) = liftIO $ toBool <$>
  [C.block| bool {
    return $(skia::textlayout::StrutStyle* strut)->getStrutEnabled();
  }|]

-- | Forces the strut height.
setForceStrutHeight :: (MonadIO m) => StrutStyle -> Bool -> m ()
setForceStrutHeight (ptr -> strut) (fromBool -> force) = liftIO $
  [C.block| void {
    $(skia::textlayout::StrutStyle* strut)->setForceStrutHeight($(bool force));
  }|]

-- | Checks if strut height is forced.
getForceStrutHeight :: (MonadIO m) => StrutStyle -> m Bool
getForceStrutHeight (ptr -> strut) = liftIO $ toBool <$>
  [C.block| bool {
    return $(skia::textlayout::StrutStyle* strut)->getForceStrutHeight();
  }|]

-- | Overrides the height.
setHeightOverride :: (MonadIO m) => StrutStyle -> Bool -> m ()
setHeightOverride (ptr -> strut) (fromBool -> override) = liftIO $
  [C.block| void {
    $(skia::textlayout::StrutStyle* strut)->setHeightOverride($(bool override));
  }|]

-- | Checks if height override is enabled.
getHeightOverride :: (MonadIO m) => StrutStyle -> m Bool
getHeightOverride (ptr -> strut) = liftIO $ toBool <$>
  [C.block| bool {
    return $(skia::textlayout::StrutStyle* strut)->getHeightOverride();
  }|]

-- | Sets half leading for the strut.
setHalfLeading :: (MonadIO m) => StrutStyle -> Bool -> m ()
setHalfLeading (ptr -> strut) (fromBool -> halfLeading) = liftIO $
  [C.block| void {
    $(skia::textlayout::StrutStyle* strut)->setHalfLeading($(bool halfLeading));
  }|]

-- | Checks if half leading is enabled.
getHalfLeading :: (MonadIO m) => StrutStyle -> m Bool
getHalfLeading (ptr -> strut) = liftIO $ toBool <$>
  [C.block| bool {
    return $(skia::textlayout::StrutStyle* strut)->getHalfLeading();
  }|]
