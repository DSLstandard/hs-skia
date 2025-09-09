module Skia.SkParagraph.Paragraph where

import Data.Traversable
import Language.C.Inline.Cpp qualified as C
import Skia.SkParagraph.DartTypes
import Skia.SkParagraph.Internal.Prelude
import Skia.SkParagraph.Metrics
import Skia.SkRect

C.context $ C.cppCtx <> C.vecCtx <> cppSkiaObjectTypes <> cppSkParagraphObjectTypes

C.include "modules/skparagraph/include/Paragraph.h"

layout ::
  (MonadIO m, IsParagraph para) =>
  para ->
  -- | Width
  Float ->
  m ()
layout (ptr . toA Paragraph -> para) (coerce -> width) = liftIO do
  [C.block| void {
    $(skia::textlayout::Paragraph* para)->layout($(float width));
  }|]

paintWithCanvas ::
  (MonadIO m, IsParagraph para, IsSkCanvas canvas) =>
  para ->
  canvas ->
  -- | (x, y)
  V2 Float ->
  m ()
paintWithCanvas (ptr . toA Paragraph -> para) (ptr . toA SkCanvas -> canvas) (coerce -> V2 x y) = liftIO do
  [C.block| void {
    $(skia::textlayout::Paragraph* para)->paint($(SkCanvas* canvas), $(float x), $(float y));
  }|]

paintWithPainter ::
  (MonadIO m, IsParagraph para, IsParagraphPainter painter) =>
  para ->
  painter ->
  -- | (x, y)
  V2 Float ->
  m ()
paintWithPainter (ptr . toA Paragraph -> para) (ptr . toA ParagraphPainter -> painter) (coerce -> V2 x y) = liftIO do
  [C.block| void {
    $(skia::textlayout::Paragraph* para)->paint($(skia::textlayout::ParagraphPainter* painter), $(float x), $(float y));
  }|]

-- | Gets the maximum width of the paragraph.
getMaxWidth :: (MonadIO m, IsParagraph para) => para -> m Float
getMaxWidth (ptr . toA Paragraph -> para) = liftIO do
  coerce
    <$> [C.exp| float {
    $(skia::textlayout::Paragraph* para)->getMaxWidth()
  }|]

-- | Gets the height of the paragraph.
getHeight :: (MonadIO m, IsParagraph para) => para -> m Float
getHeight (ptr . toA Paragraph -> para) = liftIO do
  coerce
    <$> [C.exp| float {
    $(skia::textlayout::Paragraph* para)->getHeight()
  }|]

-- | Gets the minimum intrinsic width of the paragraph.
getMinIntrinsicWidth :: (MonadIO m, IsParagraph para) => para -> m Float
getMinIntrinsicWidth (ptr . toA Paragraph -> para) = liftIO do
  coerce
    <$> [C.exp| float {
    $(skia::textlayout::Paragraph* para)->getMinIntrinsicWidth()
  }|]

-- | Gets the maximum intrinsic width of the paragraph.
getMaxIntrinsicWidth :: (MonadIO m, IsParagraph para) => para -> m Float
getMaxIntrinsicWidth (ptr . toA Paragraph -> para) = liftIO do
  coerce
    <$> [C.exp| float {
    $(skia::textlayout::Paragraph* para)->getMaxIntrinsicWidth()
  }|]

-- | Gets the alphabetic baseline of the paragraph.
getAlphabeticBaseline :: (MonadIO m, IsParagraph para) => para -> m Float
getAlphabeticBaseline (ptr . toA Paragraph -> para) = liftIO do
  coerce
    <$> [C.exp| float {
    $(skia::textlayout::Paragraph* para)->getAlphabeticBaseline()
  }|]

-- | Gets the ideographic baseline of the paragraph.
getIdeographicBaseline :: (MonadIO m, IsParagraph para) => para -> m Float
getIdeographicBaseline (ptr . toA Paragraph -> para) = liftIO do
  coerce
    <$> [C.exp| float {
    $(skia::textlayout::Paragraph* para)->getIdeographicBaseline()
  }|]

-- | Gets the longest line width in the paragraph.
getLongestLine :: (MonadIO m, IsParagraph para) => para -> m Float
getLongestLine (ptr . toA Paragraph -> para) = liftIO do
  coerce
    <$> [C.exp| float {
    $(skia::textlayout::Paragraph* para)->getLongestLine()
  }|]

-- | Checks if the paragraph exceeded the maximum lines.
didExceedMaxLines :: (MonadIO m, IsParagraph para) => para -> m Bool
didExceedMaxLines (ptr . toA Paragraph -> para) = liftIO do
  toBool
    <$> [C.exp| bool {
    $(skia::textlayout::Paragraph* para)->didExceedMaxLines()
  }|]

-- * Editor API

-- | FIXME: Skia has no documentation on this
getLineMetrics ::
  (MonadIO m, IsParagraph para) =>
  para ->
  -- | Line number
  -- | Returns 'Nothing' if target line does not exist.
  m [LineMetrics]
getLineMetrics (ptr . toA Paragraph -> para) = evalManaged do
  stdvector <-
    managed $
      bracket
        [C.block| void* {
          auto vec = new std::vector<skia::textlayout::LineMetrics>();
          $(skia::textlayout::Paragraph* para)->getLineMetrics(*vec);
          return (void*) vec;
        }|]
        ( \p ->
            [C.block| void {
          delete (std::vector<skia::textlayout::LineMetrics>*) $(void* p);
          }|]
        )

  size <- liftIO [C.exp| int { ((std::vector<skia::textlayout::LineMetrics>*) $(void* stdvector))->size() } |]

  for [0 .. (size - 1)] \i -> do
    metrics' <-
      liftIO
        [C.exp| skia::textlayout::LineMetrics* {
      ((std::vector<skia::textlayout::LineMetrics>*) $(void* stdvector))->data() + $(int i)
    }|]
    peekLineMetrics metrics'

data GlyphClusterInfo = GlyphClusterInfo
  { bounds :: Rect Float
  , clusterTextRange :: (Int, Int)
  -- ^ (Start, end)
  , glyphClusterPosition :: TextDirection
  }
  deriving (Show)

allocaGlyphClusterInfo :: Managed (Ptr C'GlyphClusterInfoRaw)
allocaGlyphClusterInfo =
  managed $
    bracket
      [C.exp| skia::textlayout::Paragraph::GlyphClusterInfo* { new skia::textlayout::Paragraph::GlyphClusterInfo() } |]
      (\p -> [C.block| void { delete $(skia::textlayout::Paragraph::GlyphClusterInfo* p); } |])

peekGlyphClusterInfo :: (MonadIO m) => Ptr C'GlyphClusterInfoRaw -> m GlyphClusterInfo
peekGlyphClusterInfo p = liftIO do
  bounds <-
    peekSkRect
      =<< [C.exp| SkRect* { &($(skia::textlayout::Paragraph::GlyphClusterInfo* p)->fBounds) } |]

  glyphClusterPosition <-
    unmarshalSkEnumOrDie @TextDirection
      =<< [C.exp| int { (int) $(skia::textlayout::Paragraph::GlyphClusterInfo* p)->fGlyphClusterPosition } |]

  start <- [C.exp| int { $(skia::textlayout::Paragraph::GlyphClusterInfo* p)->fClusterTextRange.start } |]
  end <- [C.exp| int { $(skia::textlayout::Paragraph::GlyphClusterInfo* p)->fClusterTextRange.end } |]

  pure
    GlyphClusterInfo
      { bounds = bounds
      , clusterTextRange = (fromIntegral start, fromIntegral end)
      , glyphClusterPosition = glyphClusterPosition
      }

-- | Finds the closest glyph cluster for a visual text position
getClosestGlyphClusterAt ::
  (MonadIO m, IsParagraph para) =>
  para ->
  -- | (x, y) coordinate
  V2 Float ->
  -- | 'Just' if glyph cluster was found; 'Nothing' if not (which usually
  -- means the paragraph is empty)
  m (Maybe GlyphClusterInfo)
getClosestGlyphClusterAt (ptr . toA Paragraph -> para) (coerce -> V2 x y) = evalManaged do
  p <- allocaGlyphClusterInfo

  found <-
    liftIO
      [C.block| bool {
        return $(skia::textlayout::Paragraph* para)->getClosestGlyphClusterAt(
          $(float x), $(float y),
          $(skia::textlayout::Paragraph::GlyphClusterInfo* p)
        );
      }|]

  if toBool found
    then do
      Just <$> peekGlyphClusterInfo p
    else do
      pure Nothing

-- | Finds a glyph cluster for text index
getGlyphClusterAt ::
  (MonadIO m, IsParagraph para) =>
  para ->
  -- | UTF8 code unit index
  Int ->
  -- | 'Just' if glyph cluster was found; 'Nothing' if not (which usually means
  -- the given index is invalid or out of bounds)
  m (Maybe GlyphClusterInfo)
getGlyphClusterAt (ptr . toA Paragraph -> para) (fromIntegral -> codeUnitIndex) = evalManaged do
  p <- allocaGlyphClusterInfo

  found <-
    liftIO
      [C.block| bool {
        return $(skia::textlayout::Paragraph* para)->getGlyphClusterAt(
          $(int codeUnitIndex),
          $(skia::textlayout::Paragraph::GlyphClusterInfo* p)
        );
      }|]

  if toBool found
    then do
      Just <$> peekGlyphClusterInfo p
    else do
      pure Nothing
