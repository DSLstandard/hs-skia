module Skia.SKParagraph.ParagraphStyle where

import Data.Text qualified as T
import Skia.Bindings.Skparagraph
import Skia.SKParagraph.Internal.Prelude
import Skia.SKString qualified as SKString

-- | Creates a new empty 'ParagraphStyle' object.
createEmpty :: (MonadResource m) => m (ReleaseKey, ParagraphStyle)
createEmpty =
  allocateSKObjectNeverNull
    skparagraph_paragraph_style_new
    skparagraph_paragraph_style_delete

-- | Sets the strut style for the paragraph.
setStrutStyle :: (MonadIO m) => ParagraphStyle -> StrutStyle -> m ()
setStrutStyle para strut = liftIO do
  skparagraph_paragraph_style_set_strut_style (ptr para) (ptr strut)

-- | Sets the text style for the paragraph.
setTextStyle :: (MonadIO m) => ParagraphStyle -> TextStyle -> m ()
setTextStyle paraStyle textStyle = liftIO do
  skparagraph_paragraph_style_set_text_style (ptr paraStyle) (ptr textStyle)

-- | Sets the text direction for the paragraph.
setTextDirection :: (MonadIO m) => ParagraphStyle -> TextDirection -> m ()
setTextDirection para dir = liftIO do
  skparagraph_paragraph_style_set_text_direction (ptr para) (marshalSKEnum dir)

-- | Sets the text alignment for the paragraph.
setTextAlign :: (MonadIO m) => ParagraphStyle -> TextAlign -> m ()
setTextAlign para align = liftIO do
  skparagraph_paragraph_style_set_text_align (ptr para) (marshalSKEnum align)

{- | Sets the maximum number of lines for the paragraph. Set to 'Nothing' to
indicate that it should be limitless.
-}
setMaxLines :: (MonadIO m) => ParagraphStyle -> Maybe Int -> m ()
setMaxLines para maxLines = liftIO do
  -- NOTE: Google Skia defines:
  --
  -- bool ParagraphStyle::unlimited_lines() const {
  --     return fLinesLimit == std::numeric_limits<size_t>::max();
  -- }
  skparagraph_paragraph_style_set_max_lines (ptr para) (maybe maxBound fromIntegral maxLines)

{- | Sets the ellipsis string for the paragraph.

If the input string is empty, no ellipsis is applied and the text would wrap
into new lines.

If the input string is not empty, ellipsis is applied and the text would
truncate and never wrap into new lines.
-}
setEllipsis :: (MonadIO m) => ParagraphStyle -> T.Text -> m ()
setEllipsis para ellipsis = liftIO $ runResourceT do
  (_, ellipsisSKString) <- SKString.createFromText ellipsis
  liftIO $ skparagraph_paragraph_style_set_ellipsis (ptr para) (ptr ellipsisSKString)

-- | Sets the height for the paragraph.
setHeight :: (MonadIO m) => ParagraphStyle -> Float -> m ()
setHeight para height = liftIO do
  skparagraph_paragraph_style_set_height (ptr para) (coerce height)

-- | Sets the text height behavior for the paragraph.
setTextHeightBehavior :: (MonadIO m) => ParagraphStyle -> TextHeightBehavior -> m ()
setTextHeightBehavior para behavior = liftIO do
  skparagraph_paragraph_style_set_text_height_behavior (ptr para) (marshalSKEnum behavior)

-- | Sets whether tab characters should be replaced.
setReplaceTabCharacters :: (MonadIO m) => ParagraphStyle -> Bool -> m ()
setReplaceTabCharacters para v = liftIO do
  skparagraph_paragraph_style_set_replace_tab_characters (ptr para) (fromBool v)

-- | Sets whether to apply a rounding hack.
setApplyRoundingHack :: (MonadIO m) => ParagraphStyle -> Bool -> m ()
setApplyRoundingHack para v = liftIO do
  skparagraph_paragraph_style_set_apply_rounding_hack (ptr para) (fromBool v)
