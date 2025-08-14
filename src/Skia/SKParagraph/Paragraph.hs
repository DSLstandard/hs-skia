module Skia.SKParagraph.Paragraph where

import Skia.Bindings.Skparagraph
import Skia.Bindings.Types
import Skia.Rect
import Skia.SKParagraph.Internal.Prelude
import Skia.SKParagraph.Metrics

layout ::
  (MonadIO m, IsParagraph para) =>
  para ->
  -- | Width
  Float ->
  m ()
layout (toA Paragraph -> para) width =
  liftIO $ skparagraph_paragraph_layout (ptr para) (coerce width)

paintWithCanvas ::
  (MonadIO m, IsParagraph para, IsSKCanvas canvas) =>
  para ->
  canvas ->
  -- | (x, y)
  V2 Float ->
  m ()
paintWithCanvas (toA Paragraph -> para) (toA SKCanvas -> canvas) (V2 x y) =
  liftIO $ skparagraph_paragraph_paint_with_canvas (ptr para) (ptr canvas) (coerce x) (coerce y)

paintWithPainter ::
  (MonadIO m, IsParagraph para, IsParagraphPainter painter) =>
  para ->
  painter ->
  -- | (x, y)
  V2 Float ->
  m ()
paintWithPainter (toA Paragraph -> para) (toA ParagraphPainter -> painter) (V2 x y) =
  liftIO $ skparagraph_paragraph_paint_with_painter (ptr para) (ptr painter) (coerce x) (coerce y)

-- | Gets the maximum width of the paragraph.
getMaxWidth :: (MonadIO m) => Paragraph -> m Float
getMaxWidth paragraph = liftIO $ coerce <$> skparagraph_paragraph_get_max_width (ptr paragraph)

-- | Gets the height of the paragraph.
getHeight :: (MonadIO m) => Paragraph -> m Float
getHeight paragraph = liftIO $ coerce <$> skparagraph_paragraph_get_height (ptr paragraph)

-- | Gets the minimum intrinsic width of the paragraph.
getMinIntrinsicWidth :: (MonadIO m) => Paragraph -> m Float
getMinIntrinsicWidth paragraph = liftIO $ coerce <$> skparagraph_paragraph_get_min_intrinsic_width (ptr paragraph)

-- | Gets the maximum intrinsic width of the paragraph.
getMaxIntrinsicWidth :: (MonadIO m) => Paragraph -> m Float
getMaxIntrinsicWidth paragraph = liftIO $ coerce <$> skparagraph_paragraph_get_max_intrinsic_width (ptr paragraph)

-- | Gets the alphabetic baseline of the paragraph.
getAlphabeticBaseline :: (MonadIO m) => Paragraph -> m Float
getAlphabeticBaseline paragraph = liftIO $ coerce <$> skparagraph_paragraph_get_alphabetic_baseline (ptr paragraph)

-- | Gets the ideographic baseline of the paragraph.
getIdeographicBaseline :: (MonadIO m) => Paragraph -> m Float
getIdeographicBaseline paragraph = liftIO $ coerce <$> skparagraph_paragraph_get_ideographic_baseline (ptr paragraph)

-- | Gets the longest line width in the paragraph.
getLongestLine :: (MonadIO m) => Paragraph -> m Float
getLongestLine paragraph = liftIO $ coerce <$> skparagraph_paragraph_get_longest_line (ptr paragraph)

-- | Checks if the paragraph exceeded the maximum lines.
didExceedMaxLines :: (MonadIO m) => Paragraph -> m Bool
didExceedMaxLines paragraph = liftIO do
  toBool <$> skparagraph_paragraph_did_exceed_max_lines (ptr paragraph)

-- * Editor API

getLineMetricsAt ::
  (MonadIO m) =>
  Paragraph ->
  -- | Line number
  Int ->
  -- | Returns 'Nothing' if target line does not exist.
  m (Maybe LineMetrics)
getLineMetricsAt para lineNumber = evalManaged do
  metrics' <- managed alloca
  ok <- liftIO $ skparagraph_paragraph_get_line_metrics_at (ptr para) (fromIntegral lineNumber) metrics'
  if toBool ok
    then do
      metrics <- peekWith unmarshalLineMetrics metrics'
      pure $ Just metrics
    else do
      pure Nothing

data GlyphClusterInfo = GlyphClusterInfo
  { bounds :: Rect Float
  , clusterTextRange :: (Int, Int)
  -- ^ (start, end)
  , glyphClusterPosition :: TextDirection
  }
  deriving (Show)

getClosestGlyphClusterAt ::
  (MonadIO m) =>
  Paragraph ->
  V2 Float ->
  m (Maybe GlyphClusterInfo)
getClosestGlyphClusterAt para (V2 x y) = evalManaged do
  glyphInfo' <- managed alloca
  ok <- liftIO $ skparagraph_paragraph_get_closest_glyph_cluster_at (ptr para) (coerce x) (coerce y) glyphInfo'
  if toBool ok
    then do
      glyphInfo <- unmarshalGlyphClusterInfo =<< peekWith id glyphInfo'
      pure $ Just glyphInfo
    else do
      pure Nothing

getGlyphClusterAt ::
  (MonadIO m) =>
  Paragraph ->
  -- | UTF8 code unit index
  Int ->
  m (Maybe GlyphClusterInfo)
getGlyphClusterAt para codeUnitIndex = evalManaged do
  glyphInfo' <- managed alloca
  ok <- liftIO $ skparagraph_paragraph_get_glyph_cluster_at (ptr para) (fromIntegral codeUnitIndex) glyphInfo'
  if toBool ok
    then do
      glyphInfo <- unmarshalGlyphClusterInfo =<< peekWith id glyphInfo'
      pure $ Just glyphInfo
    else do
      pure Nothing

-- * Marshal utils

unmarshalGlyphClusterInfo :: (MonadIO m) => Skparagraph_glyph_cluster_info -> m GlyphClusterInfo
unmarshalGlyphClusterInfo i = do
  glyphClusterPosition <- unmarshalSKEnumOrDie i.fGlyphClusterPosition
  pure
    GlyphClusterInfo
      { bounds = fromSKRect i.fBounds
      , clusterTextRange = (fromIntegral i.fClusterTextRange_start, fromIntegral i.fClusterTextRange_end)
      , glyphClusterPosition = glyphClusterPosition
      }
