module Skia.SkParagraph.ParagraphStyle where

import Data.Text qualified as T
import Skia.SkParagraph.DartTypes
import Skia.SkParagraph.Internal.Prelude
import Skia.SkString qualified as SkString
import qualified Language.C.Inline.Cpp as C

C.context $ C.cppCtx <> cppSkiaObjectTypes <> cppSkParagraphObjectTypes

C.include "modules/skparagraph/include/ParagraphStyle.h"

-- | Creates a new empty 'ParagraphStyle' object.
createEmpty :: (MonadResource m) => m (ReleaseKey, ParagraphStyle)
createEmpty =
  allocateSkObjectNeverNull
    [C.exp| skia::textlayout::ParagraphStyle* { new skia::textlayout::ParagraphStyle() } |]
    (\p -> [C.block| void { delete $(skia::textlayout::ParagraphStyle* p); } |])

-- | Sets the strut style for the paragraph.
setStrutStyle :: (MonadIO m) => ParagraphStyle -> StrutStyle -> m ()
setStrutStyle (ptr -> para) (ptr -> strut) = liftIO $
  [C.block| void {
    $(skia::textlayout::ParagraphStyle* para)->setStrutStyle(*$(skia::textlayout::StrutStyle* strut));
  } |]

-- | Sets the text style for the paragraph.
setTextStyle :: (MonadIO m) => ParagraphStyle -> TextStyle -> m ()
setTextStyle (ptr -> para) (ptr -> textStyle) = liftIO $
  [C.block| void {
    $(skia::textlayout::ParagraphStyle* para)->setTextStyle(*$(skia::textlayout::TextStyle* textStyle));
  } |]

-- | Sets the text direction for the paragraph.
setTextDirection :: (MonadIO m) => ParagraphStyle -> TextDirection -> m ()
setTextDirection (ptr -> para) (marshalSkEnum -> dir) = liftIO $
  [C.block| void {
    $(skia::textlayout::ParagraphStyle* para)->setTextDirection(
      static_cast<skia::textlayout::TextDirection>($(int dir)));
  } |]

-- | Sets the text alignment for the paragraph.
setTextAlign :: (MonadIO m) => ParagraphStyle -> TextAlign -> m ()
setTextAlign (ptr -> para) (marshalSkEnum -> align) = liftIO $
  [C.block| void {
    $(skia::textlayout::ParagraphStyle* para)->setTextAlign(
      static_cast<skia::textlayout::TextAlign>($(int align)));
  } |]

{- | Sets the maximum number of lines for the paragraph. Set to 'Nothing' to
indicate that it should be limitless.
-}
setMaxLines :: (MonadIO m) => ParagraphStyle -> Maybe Int -> m ()
setMaxLines (ptr -> para) (maybe maxBound fromIntegral -> maxLines) = liftIO $
  [C.block| void {
    $(skia::textlayout::ParagraphStyle* para)->setMaxLines($(size_t maxLines));
  } |]

{- | Sets the ellipsis string for the paragraph.

If the input string is empty, no ellipsis is applied and the text would wrap
into new lines.

If the input string is not empty, ellipsis is applied and the text would
truncate and never wrap into new lines.
-}
setEllipsis :: (MonadIO m) => ParagraphStyle -> T.Text -> m ()
setEllipsis (ptr -> para) ellipsis = liftIO $ runResourceT do
  (_, ptr -> ellipsisSkString) <- SkString.createFromText ellipsis
  liftIO $
    [C.block| void {
      $(skia::textlayout::ParagraphStyle* para)->setEllipsis(*$(SkString* ellipsisSkString));
    } |]

-- | Sets the height for the paragraph.
setHeight :: (MonadIO m) => ParagraphStyle -> Float -> m ()
setHeight (ptr -> para) (coerce -> height) = liftIO $
  [C.block| void {
    $(skia::textlayout::ParagraphStyle* para)->setHeight($(float height));
  } |]

-- | Sets the text height behavior for the paragraph.
setTextHeightBehavior :: (MonadIO m) => ParagraphStyle -> TextHeightBehavior -> m ()
setTextHeightBehavior (ptr -> para) (marshalSkEnum -> behavior) = liftIO $
  [C.block| void {
    $(skia::textlayout::ParagraphStyle* para)->setTextHeightBehavior(
      static_cast<skia::textlayout::TextHeightBehavior>($(int behavior)));
  } |]

-- | Sets whether tab characters should be replaced.
setReplaceTabCharacters :: (MonadIO m) => ParagraphStyle -> Bool -> m ()
setReplaceTabCharacters (ptr -> para) (fromBool -> v) = liftIO $
  [C.block| void {
    $(skia::textlayout::ParagraphStyle* para)->setReplaceTabCharacters($(bool v));
  } |]

-- | Sets whether to apply a rounding hack.
setApplyRoundingHack :: (MonadIO m) => ParagraphStyle -> Bool -> m ()
setApplyRoundingHack (ptr -> para) (fromBool -> v) = liftIO $
  [C.block| void {
    $(skia::textlayout::ParagraphStyle* para)->setApplyRoundingHack($(bool v));
  } |]
