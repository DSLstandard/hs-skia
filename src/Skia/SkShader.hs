module Skia.SkShader (
  -- * Basic
  createEmpty,
  createColor,
  createColorRGBA,
  createBlend,
  createBlendByMode,

  ---- * Gradient
  Gradient (..),
  createGradient,
  createGradientRGBA,

  ---- * Noise
  --createFractualNoise,
  --createTurbulence,

  ---- * Transformed
  --makeWithLocalMatrix,
  --makeWithColorFilter,
)
where

import Data.Vector qualified as V
import Language.C.Inline.Cpp qualified as C
import Linear
import Skia.Internal.Prelude
import Skia.Linear
import Skia.SkBlendMode
import Skia.SkColor
import Skia.SkTileMode

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "effects/SkGradientShader.h"
C.include "core/SkShader.h"
C.include "core/SkBlender.h"

-- | Creates an empty shader.
createEmpty :: (MonadResource m) => m (ReleaseKey, SkShader)
createEmpty =
  allocateSkObjectNeverNull
    [C.exp| SkShader* { SkShaders::Empty().release() } |]
    (\p -> [C.block| void { delete $(SkShader* p); } |])

-- | Creates a shader from a color.
createColor :: (MonadResource m) => SkColor -> m (ReleaseKey, SkShader)
createColor (SkColor color) =
  allocateSkObjectNeverNull
    [C.exp| SkShader* { SkShaders::Color((SkColor) $(uint32_t color)).release() } |]
    (\p -> [C.block| void { delete $(SkShader* p); } |])

-- | Creates a shader from an RGBA with a specified color space.
createColorRGBA :: (MonadResource m) => RGBA Float -> Maybe SkColorSpace -> m (ReleaseKey, SkShader)
createColorRGBA (coerce -> RGBA r g b a) (ptrOrNull -> cs) =
  allocateSkObjectNeverNull
    [C.block| SkShader* {
      SkColor4f color = {$(float r), $(float g), $(float b), $(float a)};
      return SkShaders::Color(color, sk_ref_sp($(SkColorSpace* cs))).release();
    }|]
    (\p -> [C.block| void { delete $(SkShader* p); } |])

createBlend ::
  (MonadResource m) =>
  SkBlender ->
  -- | Blend destination shader
  SkShader ->
  -- | Blend source shader
  SkShader ->
  m (ReleaseKey, SkShader)
createBlend (ptr -> blender) (ptr -> blendDst) (ptr -> blendSrc) =
  allocateSkObjectNeverNull
    [C.exp| SkShader* {
      SkShaders::Blend(
        sk_ref_sp($(SkBlender* blender)),
        sk_ref_sp($(SkShader* blendDst)),
        sk_ref_sp($(SkShader* blendSrc))
      ).release()
    } |]
    (\p -> [C.block| void { delete $(SkShader* p); } |])

createBlendByMode ::
  (MonadResource m) =>
  SkBlendMode ->
  -- | Blend destination shader
  SkShader ->
  -- | Blend source shader
  SkShader ->
  m (ReleaseKey, SkShader)
createBlendByMode (marshalSkEnum -> mode) (ptr -> blendDst) (ptr -> blendSrc) =
  allocateSkObjectNeverNull
    [C.exp| SkShader* {
      SkShaders::Blend(
        (SkBlendMode)$(int mode),
        sk_ref_sp($(SkShader* blendDst)),
        sk_ref_sp($(SkShader* blendSrc))
      ).release()
    } |]
    (\p -> [C.block| void { delete $(SkShader* p); } |])

data Gradient
  = Gradient'Linear
      { start :: V2 Float
      , end :: V2 Float
      }
  | Gradient'Radial
      { center :: V2 Float
      , radius :: Float
      }
  | Gradient'Sweep
      { center :: V2 Float
      , startAngleDegrees :: Float
      , endAngleDegrees :: Float
      }
  | Gradient'TwoPointConical
      { start :: V2 Float
      , startRadius :: Float
      , end :: V2 Float
      , endRadius :: Float
      }
  deriving (Show, Eq, Ord)

data PrivInColor color hex
  = PrivInColor color (Maybe SkColorSpace)
  | PrivInColor'Hex hex
  deriving (Show)

privCreateGradientRaw ::
  (MonadIO m) =>
  Gradient ->
  -- | Color array
  PrivInColor (Ptr C'SkColor4f) (Ptr SkColor) ->
  -- | Color position array
  Ptr Float ->
  -- | Color count
  Int ->
  SkTileMode ->
  -- | Local matrix
  Maybe (M33 Float) ->
  m (Ptr C'SkShader)
privCreateGradientRaw gradient inColorArray poses count tileMode localMatrix = evalManaged do
  -- NOTE: This function is implemented in such a way so all 4 gradient types
  -- can be implemented in < 30 lines, and not >= 500 lines.

  localMatrix' <- maybe (pure nullPtr) marshalSkMatrix localMatrix
  let tileMode' :: CInt = fromIntegral $ marshalSkEnum tileMode
  let poses' :: Ptr CFloat = coercePtr poses
  let count' :: CInt = fromIntegral count

  -- Pass gradient type specific arguments
  (newGradientColorHex, newGradientColor4f) <- case gradient of
    Gradient'Linear{start, end} -> do
      start' <- marshalSkPoint start
      end' <- marshalSkPoint end

      pure
        ( \skcolors' -> do
            [C.block| SkShader* {
              SkPoint points[2] = { *$(SkPoint* start'), *$(SkPoint* end') };
              return SkGradientShader::MakeLinear(
                points,
                (SkColor*) $(uint32_t* skcolors'),
                $(float* poses'), $(int count'), (SkTileMode) $(int tileMode'), 0, $(SkMatrix* localMatrix')
              ).release();
            }|]
        , \skcolor4fs' colorspace' -> do
            [C.block| SkShader* {
              SkPoint points[2] = { *$(SkPoint* start'), *$(SkPoint* end') };
              return SkGradientShader::MakeLinear(
                points,
                $(SkColor4f* skcolor4fs'), sk_ref_sp($(SkColorSpace* colorspace')),
                $(float* poses'), $(int count'), (SkTileMode) $(int tileMode'), 0, $(SkMatrix* localMatrix')
              ).release();
            }|]
        )
    Gradient'Radial{center, radius} -> do
      center' <- marshalSkPoint center
      let radius' :: CFloat = coerce radius
      pure
        ( \skcolors' -> do
            [C.block| SkShader* {
              return SkGradientShader::MakeRadial(
                *$(SkPoint* center'), $(float radius'),
                (SkColor*) $(uint32_t* skcolors'),
                $(float* poses'), $(int count'), (SkTileMode) $(int tileMode'), 0, $(SkMatrix* localMatrix')
              ).release();
            }|]
        , \skcolor4fs' colorspace' -> do
            [C.block| SkShader* {
              return SkGradientShader::MakeRadial(
                *$(SkPoint* center'), $(float radius'),
                $(SkColor4f* skcolor4fs'), sk_ref_sp($(SkColorSpace* colorspace')),
                $(float* poses'), $(int count'), (SkTileMode) $(int tileMode'), 0, $(SkMatrix* localMatrix')
              ).release();
            }|]
        )
    Gradient'TwoPointConical{start, startRadius = coerce -> startRadius, end, endRadius = coerce -> endRadius} -> do
      start' <- marshalSkPoint start
      end' <- marshalSkPoint end
      pure
        ( \skcolors' -> do
            [C.block| SkShader* {
              return SkGradientShader::MakeTwoPointConical(
                *$(SkPoint* start'), $(float startRadius),
                *$(SkPoint* end'), $(float endRadius),
                (SkColor*) $(uint32_t* skcolors'),
                $(float* poses'), $(int count'), (SkTileMode) $(int tileMode'), 0, $(SkMatrix* localMatrix')
              ).release();
            }|]
        , \skcolor4fs' colorspace' -> do
            [C.block| SkShader* {
              return SkGradientShader::MakeTwoPointConical(
                *$(SkPoint* start'), $(float startRadius),
                *$(SkPoint* end'), $(float endRadius),
                $(SkColor4f* skcolor4fs'), sk_ref_sp($(SkColorSpace* colorspace')),
                $(float* poses'), $(int count'), (SkTileMode) $(int tileMode'), 0, $(SkMatrix* localMatrix')
              ).release();
            }|]
        )
    Gradient'Sweep{center = coerce -> V2 cx cy, startAngleDegrees = coerce -> startAngle, endAngleDegrees = coerce -> endAngle} -> do
      pure
        ( \skcolors' -> do
            [C.block| SkShader* {
              return SkGradientShader::MakeSweep(
                $(float cx), $(float cy),
                (SkColor*) $(uint32_t* skcolors'),
                $(float* poses'), $(int count'), (SkTileMode) $(int tileMode'),
                $(float startAngle), $(float endAngle),
                0, $(SkMatrix* localMatrix')
              ).release();
            }|]
        , \skcolor4fs' colorspace' -> do
            [C.block| SkShader* {
              return SkGradientShader::MakeSweep(
                $(float cx), $(float cy),
                $(SkColor4f* skcolor4fs'), sk_ref_sp($(SkColorSpace* colorspace')),
                $(float* poses'), $(int count'), (SkTileMode) $(int tileMode'),
                $(float startAngle), $(float endAngle),
                0, $(SkMatrix* localMatrix')
              ).release();
            }|]
        )

  -- Pass color specific arguments
  case inColorArray of
    PrivInColor'Hex skcolors' -> do
      liftIO $ newGradientColorHex (coercePtr skcolors')
    PrivInColor skcolor4fs' colorspace -> do
      liftIO $ newGradientColor4f skcolor4fs' (ptrOrNull colorspace)

-- | Creates a gradient shader from a list of colors and positions.
createGradient ::
  (MonadResource m) =>
  Gradient ->
  -- | List of (color position, color hex)
  [(Float, SkColor)] ->
  SkTileMode ->
  -- | Local matrix
  Maybe (M33 Float) ->
  m (ReleaseKey, SkShader)
createGradient gradient entries tileMode localMatrix =
  allocateSkObjectNeverNull
    ( evalManaged do
        let (poses, skcolors) = unzip entries
        (poses', numEntries) <- managed $ withArrayLen' poses
        skcolors' <- managed $ withArray skcolors
        liftIO $ privCreateGradientRaw gradient (PrivInColor'Hex skcolors') poses' numEntries tileMode localMatrix
    )
    (\p -> [C.block| void { delete $(SkShader* p); } |])

-- | Like 'createGradient' but uses RGBA colors instead of 'SkColor'.
createGradientRGBA ::
  (MonadResource m) =>
  Gradient ->
  -- | List of (color position, color rgba)
  [(Float, RGBA Float)] ->
  Maybe SkColorSpace ->
  SkTileMode ->
  -- | Local matrix
  Maybe (M33 Float) ->
  m (ReleaseKey, SkShader)
createGradientRGBA gradient entries colorspace tileMode localMatrix =
  allocateSkObjectNeverNull
    ( evalManaged do
        let (poses, skcolor4fs) = unzip entries
        (poses', numEntries) <- managed $ withArrayLen' poses
        skcolor4fs' <- marshalSkColor4fArray (V.fromList skcolor4fs)
        privCreateGradientRaw gradient (PrivInColor skcolor4fs' colorspace) poses' numEntries tileMode localMatrix
    )
    (\p -> [C.block| void { delete $(SkShader* p); } |])

{-
{- | This will construct Perlin noise of the given type (Fractal Noise or
Turbulence).

Both base frequencies (X and Y) have a usual range of (0..1) and must be
non-negative.

The number of octaves provided should be fairly small, with a limit of 255
enforced. Each octave doubles the frequency, so 10 octaves would produce noise
from baseFrequency * 1, * 2, * 4, ..., * 512, which quickly yields
insignificantly small periods and resembles regular unstructured noise rather
than Perlin noise.

If tileSize isn't NULL or an empty size, the tileSize parameter will be used to
modify the frequencies so that the noise will be tileable for the given tile
size. If tileSize is NULL or an empty size, the frequencies will be used as is
without modification.
-}
createFractualNoise ::
  (MonadResource m) =>
  -- | Base frequency X, Y
  V2 Float ->
  -- | Num octaves
  Int ->
  -- | Seed
  Float ->
  -- | Tile size
  Maybe (V2 Int) ->
  m (ReleaseKey, SkShader)
createFractualNoise (V2 fx fy) numOctaves seed tileSize =
  allocateSkObjectNeverNull
    ( evalManaged do
        tileSize' <- useNullIfNothing storable $ toSKISize <$> tileSize
        liftIO $ sk_shader_new_perlin_noise_fractal_noise (coerce fx) (coerce fy) (fromIntegral numOctaves) (coerce seed) tileSize'
    )
    sk_shader_unref

{- | This will construct Perlin noise of the given type (Fractal Noise or
Turbulence).

Both base frequencies (X and Y) have a usual range of (0..1) and must be
non-negative.

The number of octaves provided should be fairly small, with a limit of 255
enforced. Each octave doubles the frequency, so 10 octaves would produce noise
from baseFrequency * 1, * 2, * 4, ..., * 512, which quickly yields
insignificantly small periods and resembles regular unstructured noise rather
than Perlin noise.

If tileSize isn't NULL or an empty size, the tileSize parameter will be used to
modify the frequencies so that the noise will be tileable for the given tile
size. If tileSize is NULL or an empty size, the frequencies will be used as is
without modification.
-}
createTurbulence ::
  (MonadResource m) =>
  -- | Base frequency X, Y
  V2 Float ->
  -- | Num octaves
  Int ->
  -- | Seed
  Float ->
  -- | Tile size
  Maybe (V2 Int) ->
  m (ReleaseKey, SkShader)
createTurbulence (V2 fx fy) numOctaves seed tileSize =
  allocateSkObjectNeverNull
    ( evalManaged do
        tileSize' <- useNullIfNothing storable $ toSKISize <$> tileSize
        liftIO $ sk_shader_new_perlin_noise_turbulence (coerce fx) (coerce fy) (fromIntegral numOctaves) (coerce seed) tileSize'
    )
    sk_shader_unref

{- | Return a shader that will apply the specified localMatrix to this shader.
The specified matrix will be applied before any matrix associated with this
shader.
-}
makeWithLocalMatrix :: (MonadResource m) => SkShader -> M33 Float -> m (ReleaseKey, SkShader)
makeWithLocalMatrix shader localMatrix =
  allocateSkObjectNeverNull
    ( evalManaged do
        localMatrix' <- storable $ toSkMatrix localMatrix
        liftIO $ sk_shader_with_local_matrix (ptr shader) localMatrix'
    )
    sk_shader_unref

{- | Create a new shader that produces the same colors as invoking this shader
and then applying the colorfilter.
-}
makeWithColorFilter :: (MonadResource m) => SkShader -> SkColorFilter -> m (ReleaseKey, SkShader)
makeWithColorFilter shader colorFilter =
  allocateSkObjectNeverNull
    ( evalManaged do
        liftIO $ sk_shader_with_color_filter (ptr shader) (ptr colorFilter)
    )
    sk_shader_unref

-}