module Skia.SKFont where

import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Char
import Data.Int
import Data.Text qualified as T
import Data.Text.Foreign qualified as T
import Data.Vector.Storable qualified as VS
import Foreign.ForeignPtr
import Skia.Bindings.Sk_font
import Skia.Bindings.Sk_typeface
import Skia.Bindings.Types
import Skia.Internal.Prelude
import Skia.Rect

-- | Creates an 'SKFont' with default internal settings.
createEmpty :: (MonadResource m) => m (ReleaseKey, SKFont)
createEmpty =
  allocateSKObjectNeverNull
    sk_font_new
    sk_font_delete

{- | Creates an 'SKFont' with default internal settings + input SkTypeface and
size in points, horizontal scale, and horizontal skew. Horizontal scale emulates
condensed and expanded fonts. Horizontal skew emulates oblique fonts.
-}
createWith ::
  (MonadResource m) =>
  SKTypeface ->
  -- | size; typographic height of text
  Float ->
  -- | scaleX; text horizontal scale
  --
  -- TIP: You may set this to 1.0 as a default.
  Float ->
  -- | skewX; additional shear on x-axis relative to y-axis
  --
  -- TIP: You may set this to 0.0 as a default.
  Float ->
  m (ReleaseKey, SKFont)
createWith tf size scaleX skewX =
  allocateSKObjectNeverNull
    (sk_font_new_with_values (ptr tf) (coerce size) (coerce scaleX) (coerce skewX))
    sk_font_delete

isForceAutoHinting :: (MonadIO m) => SKFont -> m Bool
isForceAutoHinting font =
  liftIO $ fmap toBool $ sk_font_is_force_auto_hinting (ptr font)

setForceAutoHinting :: (MonadIO m) => SKFont -> Bool -> m ()
setForceAutoHinting font v =
  liftIO $ sk_font_set_force_auto_hinting (ptr font) (fromBool v)

isEmbeddedBitmaps :: (MonadIO m) => SKFont -> m Bool
isEmbeddedBitmaps font =
  liftIO $ fmap toBool $ sk_font_is_embedded_bitmaps (ptr font)

setEmbeddedBitmaps :: (MonadIO m) => SKFont -> Bool -> m ()
setEmbeddedBitmaps font v =
  liftIO $ sk_font_set_embedded_bitmaps (ptr font) (fromBool v)

isSubpixel :: (MonadIO m) => SKFont -> m Bool
isSubpixel font =
  liftIO $ fmap toBool $ sk_font_is_subpixel (ptr font)

setSubpixel :: (MonadIO m) => SKFont -> Bool -> m ()
setSubpixel font v =
  liftIO $ sk_font_set_subpixel (ptr font) (fromBool v)

isLinearMetrics :: (MonadIO m) => SKFont -> m Bool
isLinearMetrics font =
  liftIO $ fmap toBool $ sk_font_is_linear_metrics (ptr font)

setLinearMetrics :: (MonadIO m) => SKFont -> Bool -> m ()
setLinearMetrics font v =
  liftIO $ sk_font_set_linear_metrics (ptr font) (fromBool v)

isEmbolden :: (MonadIO m) => SKFont -> m Bool
isEmbolden font =
  liftIO $ fmap toBool $ sk_font_is_embolden (ptr font)

setEmbolden :: (MonadIO m) => SKFont -> Bool -> m ()
setEmbolden font v =
  liftIO $ sk_font_set_embolden (ptr font) (fromBool v)

isBaselineSnap :: (MonadIO m) => SKFont -> m Bool
isBaselineSnap font =
  liftIO $ fmap toBool $ sk_font_is_baseline_snap (ptr font)

setBaselineSnap :: (MonadIO m) => SKFont -> Bool -> m ()
setBaselineSnap font v =
  liftIO $ sk_font_set_baseline_snap (ptr font) (fromBool v)

getEdging :: (MonadIO m) => SKFont -> m SKFontEdging
getEdging font =
  liftIO $ unmarshalSKEnumOrDie =<< sk_font_get_edging (ptr font)

setEdging :: (MonadIO m) => SKFont -> SKFontEdging -> m ()
setEdging font v =
  liftIO $ sk_font_set_edging (ptr font) (marshalSKEnum v)

getHinting :: (MonadIO m) => SKFont -> m SKFontHinting
getHinting font =
  liftIO $ unmarshalSKEnumOrDie =<< sk_font_get_hinting (ptr font)

setHinting :: (MonadIO m) => SKFont -> SKFontHinting -> m ()
setHinting font v =
  liftIO $ sk_font_set_hinting (ptr font) (marshalSKEnum v)

{- | Returns SkTypeface if set, or 'Nothing'.

This function increments the reference counter of the returned typeface by one,
and decrements the reference counter once the returned typeface is finalized in
Haskell.
-}
getTypeface :: (MonadResource m) => SKFont -> m (Maybe (ReleaseKey, SKTypeface))
getTypeface font =
  -- NOTE: sk_font_get_typeface already increments the refcnt
  allocateSKObjectOrNothingIfNull
    (sk_font_get_typeface (ptr font))
    sk_typeface_unref

{- | Sets SkTypeface to typeface, decreasing SkRefCnt of the previous
SkTypeface. Pass 'Nothing' to clear SkTypeface and use the default typeface.

Increments the reference counter of the input typeface by one by 'SKFont'.
-}
setTypeface :: (MonadIO m) => SKFont -> Maybe SKTypeface -> m ()
setTypeface font tf =
  liftIO $ sk_font_set_typeface (ptr font) (ptrOrNull tf)

-- | Returns text size in points.
getSize :: (MonadIO m) => SKFont -> m Float
getSize font =
  fmap coerce $ liftIO $ sk_font_get_size (ptr font)

{- | Sets text size in points. Has no effect if textSize is not greater than or
equal to zero.
-}
setSize :: (MonadIO m) => SKFont -> Float -> m ()
setSize font v =
  liftIO $ sk_font_set_size (ptr font) (coerce v)

-- | Returns text scale on x-axis. Default value is 1.
getScaleX :: (MonadIO m) => SKFont -> m Float
getScaleX font =
  liftIO $ fmap coerce $ sk_font_get_scale_x (ptr font)

-- | Sets text scale on x-axis. Default value is 1.
setScaleX :: (MonadIO m) => SKFont -> Float -> m ()
setScaleX font v =
  liftIO $ sk_font_set_scale_x (ptr font) (coerce v)

-- | Returns text skew on x-axis. Default value is zero.
getSkewX :: (MonadIO m) => SKFont -> m Float
getSkewX font =
  liftIO $ fmap coerce $ sk_font_get_skew_x (ptr font)

-- | Sets text skew on x-axis. Default value is zero.
setSkewX ::
  (MonadIO m) =>
  SKFont ->
  -- | \"skewX\". Additional shear on x-axis relative to y-axis
  Float ->
  m ()
setSkewX font v =
  liftIO $ sk_font_set_skew_x (ptr font) (coerce v)

{- | Converts text into glyph indices.

Returns the number of glyph indices represented by text. SkTextEncoding
specifies how text represents characters or glyphs. glyphs may be nullptr, to
compute the glyph count.

Does not check text for valid character codes or valid glyph indices.

If byteLength equals zero, returns zero. If byteLength includes a partial
character, the partial character is ignored.

If encoding is SkTextEncoding::kUTF8 and text contains an invalid UTF-8
sequence, zero is returned.

When encoding is SkTextEncoding::kUTF8, SkTextEncoding::kUTF16, or
SkTextEncoding::kUTF32; then each Unicode codepoint is mapped to a single glyph.
This function uses the default character-to-glyph mapping from the SkTypeface
and maps characters not found in the SkTypeface to zero.
-}
textToGlyphsRaw ::
  (MonadIO m) =>
  SKFont ->
  -- | Text data
  BS.ByteString ->
  -- | Text encoding
  SKTextEncoding ->
  -- | Destination glyph array
  Ptr GlyphId ->
  -- | Destination glyph array capacity
  Int ->
  -- | Returns the number of glyphs written to destination glyph array
  m Int
textToGlyphsRaw font textData encoding glyphs maxGlyphsCount = evalManaged do
  (textBytes, textBytesLen) <- managed $ BS.unsafeUseAsCStringLen textData

  returnedCount <-
    liftIO $
      sk_font_text_to_glyphs
        (ptr font)
        (castPtr textBytes)
        (fromIntegral textBytesLen)
        (marshalSKEnum encoding)
        (castPtr glyphs)
        (fromIntegral maxGlyphsCount)
  pure $ fromIntegral returnedCount

{- | Returns glyph index for Unicode character.

If the character is not supported by the SkTypeface, returns 0.
-}
unicharToGlyph :: (MonadIO m) => SKFont -> Unichar -> m GlyphId
unicharToGlyph font code =
  liftIO $ GlyphId <$> sk_font_unichar_to_glyph (ptr font) code

-- | Like 'unicharToGlyph' by takes a Haskell 'Char'.
charToGlyph :: (MonadIO m) => SKFont -> Char -> m GlyphId
charToGlyph font char = unicharToGlyph font (fromIntegral (ord char))

-- Google Skia does not document 'unicharsToGlyphsRaw'. The comment is an
-- educated guess made by inspecting src/core/SkTypeface.cpp.

-- | Like 'unicharToGlyph' but operates on an array.
unicharsToGlyphsRaw ::
  (MonadIO m) =>
  SKFont ->
  -- | Unichars
  Ptr Unichar ->
  -- | Number of unichars
  Int ->
  -- | Destination glyph array. Its size should be at least the input number
  -- of unichars.
  Ptr GlyphId ->
  m ()
unicharsToGlyphsRaw font unichars numUnichars dstGlyphs =
  liftIO $
    sk_font_unichars_to_glyphs
      (ptr font)
      unichars
      (fromIntegral numUnichars)
      (castPtr dstGlyphs)

-- | Like 'unicharsToGlyphsRaw' but operates on Haskell vectors.
unicharVectorToGlyphs ::
  (MonadIO m) =>
  SKFont ->
  -- | Vector of unichars
  VS.Vector Unichar ->
  -- | Returns the corresponding glyph IDs of the input.
  m (VS.Vector GlyphId)
unicharVectorToGlyphs font unichars = evalManaged do
  let len = VS.length unichars
  unichars' <- storableVector unichars
  liftIO $ runResourceT do
    (releaseKey, glyphsArray) <- allocate (mallocForeignPtrArray len) finalizeForeignPtr

    liftIO $ withForeignPtr glyphsArray \glyphsArray' -> do
      unicharsToGlyphsRaw font unichars' (fromIntegral len) glyphsArray'

    release releaseKey

    pure $ VS.unsafeFromForeignPtr0 glyphsArray len

-- | Like 'unicharVectorToGlyphs' but operates on a Haskell String.
stringToGlyphs :: (MonadIO m) => SKFont -> String -> m (VS.Vector GlyphId)
stringToGlyphs font string =
  unicharVectorToGlyphs font $ VS.fromList $ fmap (fromIntegral . ord) string

{- | Modifies path to be the outline of the glyph.

If the glyph has an outline, modifies path to be the glyph's outline and returns
true. The glyph outline may be empty. Degenerate contours in the glyph outline
will be skipped. If glyph is described by a bitmap, returns false and ignores
path parameter.
-}
getPathOfGlyph ::
  (MonadIO m) =>
  SKFont ->
  GlyphId ->
  -- | Destination path
  SKPath ->
  -- | Returns true if the destination path describes the input glyph.
  m Bool
getPathOfGlyph font glyphId dstPath =
  liftIO $ fmap toBool $ sk_font_get_path (ptr font) (coerce glyphId) (ptr dstPath)

{- | Returns SkFontMetrics associated with SkTypeface.

The return value is the recommended spacing between lines: the sum of metrics
descent, ascent, and leading.

Results are scaled by text size but does not take into account dimensions
required by text scale, text skew, fake bold, style stroke, and SkPathEffect.
-}
getMetrics ::
  (MonadIO m) =>
  SKFont ->
  -- | Returns ('FontMetrics', recommended spacing between lines)
  m (FontMetrics, Float)
getMetrics font = evalManaged do
  metrics' <- managed alloca
  lineSpacing <- liftIO $ fmap coerce $ sk_font_get_metrics (ptr font) metrics'
  metrics <- peekWith marshalFontMetrics metrics'
  pure (metrics, lineSpacing)

{- | Returns the advance width of text and the bounding box of text relative to
(0, 0).

The advance is the normal distance to move before drawing additional text.

The paint stroke settings, mask filter, or path effect may modify the bounds.
-}
measureTextRaw ::
  (MonadIO m) =>
  SKFont ->
  -- | Text bytes
  Ptr Word8 ->
  -- | Text byte length
  Int ->
  -- | Text encoding
  SKTextEncoding ->
  -- | Optional paint
  Maybe SKPaint ->
  -- | Returns (the sum of the default advance widths, bounding box relative to (0, 0))
  m (Float, Rect Float)
measureTextRaw font textBytes textBytesLen encoding paint = evalManaged do
  bounds' <- managed alloca
  advance <-
    liftIO $
      fmap coerce $
        sk_font_measure_text
          (ptr font)
          (castPtr textBytes)
          (fromIntegral textBytesLen)
          (marshalSKEnum encoding)
          bounds'
          (ptrOrNull paint)
  bounds <- peekWith fromSKRect bounds'
  pure (advance, bounds)

-- | Like 'measureTextRaw' but takes 'T.Text'.
measureText :: (MonadIO m) => SKFont -> T.Text -> Maybe SKPaint -> m (Float, Rect Float)
measureText font txt paint = liftIO do
  T.withCStringLen txt \(cstr, cstrlen) -> do
    measureTextRaw font (castPtr cstr) cstrlen SKTextEncoding'UTF8 paint

-- * SKFontTableTag

newtype SKFontTableTag = SKFontTableTag {unFontTableTag :: Sk_font_table_tag}
  deriving (Show, Eq, Ord)

mkFontTableTagBy4Byte :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
mkFontTableTagBy4Byte c1 c2 c3 c4 = do
  -- FIXME: Is there already a builtin function that combines four Word8s into
  -- one Word32?
  let c1' = fromIntegral c1
  let c2' = fromIntegral c2
  let c3' = fromIntegral c3
  let c4' = fromIntegral c4
  (c1' `shiftL` 24) .|. (c2' `shiftL` 16) .|. (c3' `shiftL` 8) .|. c4'

mkFontTableTagByString :: String -> Word32
mkFontTableTagByString [c1, c2, c3, c4] = do
  mkFontTableTagBy4Byte (toByte c1) (toByte c2) (toByte c3) (toByte c4)
 where
  toByte :: Char -> Word8
  toByte ch = fromIntegral (Data.Char.ord ch)
mkFontTableTagByString _ = do
  error "Input string must have exactly 4 chars"

-- * Extra types

type Unichar = Int32

newtype GlyphId = GlyphId {unGlyphId :: Word16}
  deriving (Show, Eq, Ord)
  deriving newtype (Num, Storable)

data FontMetrics = FontMetrics
  { glyphBounds :: Maybe (Rect Float)
  -- ^ TOP = greatest extent above origin of any glyph bounding box, typically
  -- negative; deprecated with variable fonts
  --
  -- BOTTOM = greatest extent below origin of any glyph bounding box,
  -- typically positive; deprecated with variable fonts
  --
  -- LEFT = (also called xMin) greatest extent to left of origin of any glyph
  -- bounding box, typically negative; deprecated with variable fonts
  --
  -- RIGHT = (also called xMax) greatest extent to right of origin of any
  -- glyph bounding box, typically negative; deprecated with variable fonts
  , ascent :: Float
  -- ^ Distance to reserve above baseline, typically negative.
  , descent :: Float
  -- ^ Distance to reserve below baseline, typically positive.
  , leading :: Float
  -- ^ Distance to add between lines, typically positive or zero.
  , avgCharWidth :: Maybe Float
  -- ^ Average character width, 'Nothing' if unknown.
  , maxCharWidth :: Maybe Float
  -- ^ Maximum character width, 'Nothing' if unknown.
  , xHeight :: Maybe Float
  -- ^ Height of lower-case 'x', typically negative, 'Nothing' if unknown.
  , capHeight :: Maybe Float
  -- ^ Height of an upper-case letter, typically negative, 'Nothing' if unknown,
  , underlineThickness :: Maybe Float
  -- ^ Underline thickness, 'Nothing' if invalid.
  , underlinePosition :: Maybe Float
  -- ^ Distance from baseline to top of stroke, typically positive, 'Nothing' if invalid.
  , strikeoutThickness :: Maybe Float
  -- ^ Strikeout thickness, 'Nothing' if invalid.
  , strikeoutPosition :: Maybe Float
  -- ^ Distance from baseline to bottom of stroke, typically negative, 'Nothing' if invalid.
  }
  deriving (Show)

-- * Marshal utils

marshalFontMetrics :: Sk_fontmetrics -> FontMetrics
marshalFontMetrics input =
  FontMetrics
    { glyphBounds = do
        validUnlessFlagBitSet
          kBoundsInvalid_FlagBit
          Rect
            { left = coerce input.fXMin
            , right = coerce input.fXMax
            , top = coerce input.fTop
            , bottom = coerce input.fBottom
            }
    , ascent = coerce input.fAscent
    , descent = coerce input.fDescent
    , leading = coerce input.fLeading
    , avgCharWidth = validUnlessZero input.fAvgCharWidth
    , maxCharWidth = validUnlessZero input.fMaxCharWidth
    , xHeight = validUnlessZero input.fXHeight
    , capHeight = validUnlessZero input.fCapHeight
    , underlineThickness = validIfFlagBitSet kUnderlineThicknessIsValid_FlagBit (coerce input.fUnderlineThickness)
    , underlinePosition = validIfFlagBitSet kUnderlinePositionIsValid_FlagBit (coerce input.fUnderlinePosition)
    , strikeoutThickness = validIfFlagBitSet kStrikeoutThicknessIsValid_FlagBit (coerce input.fStrikeoutThickness)
    , strikeoutPosition = validIfFlagBitSet kStrikeoutPositionIsValid_FlagBit (coerce input.fStrikeoutPosition)
    }
 where
  -- Some values are set to "zero" when unknown. Example: Google Skia's
  -- comment on fXHeight: height of lower-case 'x', zero if unknown,
  -- typically negative.
  validUnlessZero :: CFloat -> Maybe Float
  validUnlessZero value =
    -- TODO: It should be fine to simply do (== 0)... right?
    if value == 0
      then Nothing
      else Just (coerce value)

  validIfFlagBitSet :: Int -> a -> Maybe a
  validIfFlagBitSet bit value = guard (testBit input.fFlags bit) $> value

  validUnlessFlagBitSet :: Int -> a -> Maybe a
  validUnlessFlagBitSet bit value = guard (not $ testBit input.fFlags bit) $> value

  -- NOTE: See Google Skia's include/core/SkFontMetrics.h's `enum FontMetricsFlags`
  kUnderlineThicknessIsValid_FlagBit = 0 -- set if fUnderlineThickness is valid
  kUnderlinePositionIsValid_FlagBit = 1 -- set if fUnderlinePosition is valid
  kStrikeoutThicknessIsValid_FlagBit = 2 -- set if fStrikeoutThickness is valid
  kStrikeoutPositionIsValid_FlagBit = 3 -- set if fStrikeoutPosition is valid
  kBoundsInvalid_FlagBit = 4 -- set if fTop, fBottom, fXMin, fXMax invalid
