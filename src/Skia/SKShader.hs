module Skia.SKShader (
  -- * Basic
  createEmpty,
  createColor,
  createColorRGBA,
  createBlend,
  createBlendByMode,

  -- * Gradient
  Gradient (..),
  createGradient,
  createGradientRGBA,

  -- * Noise
  createFractualNoise,
  createTurbulence,

  -- * Transformed
  makeWithLocalMatrix,
  makeWithColorFilter,
)
where

import Linear
import Skia.Bindings.Sk_shader
import Skia.Bindings.Types
import Skia.Color
import Skia.Internal.Prelude

-- | Creates an empty shader.
createEmpty :: (MonadResource m) => m (ReleaseKey, SKShader)
createEmpty =
  allocateSKObjectNeverNull
    sk_shader_new_empty
    sk_shader_unref

-- | Creates a shader from a color.
createColor :: (MonadResource m) => SKColor -> m (ReleaseKey, SKShader)
createColor color =
  allocateSKObjectNeverNull
    (sk_shader_new_color (coerce color))
    sk_shader_unref

-- | Creates a shader from an RGBA with a specified color space.
createColorRGBA :: (MonadResource m) => RGBA Float -> Maybe SKColorSpace -> m (ReleaseKey, SKShader)
createColorRGBA color colorspace =
  allocateSKObjectNeverNull
    ( evalManaged do
        color' <- storable $ toSKColor4f color
        liftIO $ sk_shader_new_color4f color' (ptrOrNull colorspace)
    )
    sk_shader_unref

createBlend ::
  (MonadResource m) =>
  SKBlender ->
  -- | Blend destination shader
  SKShader ->
  -- | Blend source shader
  SKShader ->
  m (ReleaseKey, SKShader)
createBlend blender blendDst blendSrc =
  allocateSKObjectNeverNull
    (sk_shader_new_blender (ptr blender) (ptr blendDst) (ptr blendSrc))
    sk_shader_unref

createBlendByMode ::
  (MonadResource m) =>
  SKBlendMode ->
  -- | Blend destination shader
  SKShader ->
  -- | Blend source shader
  SKShader ->
  m (ReleaseKey, SKShader)
createBlendByMode blendMode blendDst blendSrc =
  allocateSKObjectNeverNull
    (sk_shader_new_blend (marshalSKEnum blendMode) (ptr blendDst) (ptr blendSrc))
    sk_shader_unref

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
  = PrivInColor color (Maybe SKColorSpace)
  | PrivInColor'Hex hex
  deriving (Show)

privCreateGradientRaw ::
  (MonadIO m) =>
  Gradient ->
  -- | Color array
  PrivInColor (Ptr Sk_color4f) (Ptr SKColor) ->
  -- | Color position array
  Ptr Float ->
  -- | Color count
  Int ->
  SKShaderTileMode ->
  -- | Local matrix
  M33 Float ->
  m (Ptr Sk_shader)
privCreateGradientRaw gradient inColorArray colorPosArray colorCount tileMode localMatrix = evalManaged do
  -- NOTE: This function is implemented in such a way so all 4 gradient types
  -- can be implemented in < 30 lines, and not >= 500 lines.

  -- Pass gradient type specific arguments
  (newGradientColorHex, newGradientColor4f) <- case gradient of
    Gradient'Linear{start, end} -> do
      points' <- managed $ withArray [toSKPoint start, toSKPoint end]
      pure
        ( sk_shader_new_linear_gradient points'
        , sk_shader_new_linear_gradient_color4f points'
        )
    Gradient'Radial{center, radius} -> do
      center' <- storable (toSKPoint center)
      pure
        ( sk_shader_new_radial_gradient center' (coerce radius)
        , sk_shader_new_radial_gradient_color4f center' (coerce radius)
        )
    Gradient'Sweep{center, startAngleDegrees, endAngleDegrees} -> do
      center' <- storable (toSKPoint center)
      -- For some reason the arguments startAngle & endAngle are placed at the back...
      pure
        ( \colors colorPos colorCount tileMode localMatrix ->
            sk_shader_new_sweep_gradient center' colors colorPos colorCount tileMode (coerce startAngleDegrees) (coerce endAngleDegrees) localMatrix
        , \colors colorspace colorPos colorCount tileMode localMatrix ->
            sk_shader_new_sweep_gradient_color4f center' colors colorspace colorPos colorCount tileMode (coerce startAngleDegrees) (coerce endAngleDegrees) localMatrix
        )
    Gradient'TwoPointConical{start, startRadius, end, endRadius} -> do
      start' <- storable (toSKPoint start)
      end' <- storable (toSKPoint end)
      pure
        ( sk_shader_new_two_point_conical_gradient start' (coerce startRadius) end' (coerce endRadius)
        , sk_shader_new_two_point_conical_gradient_color4f start' (coerce startRadius) end' (coerce endRadius)
        )

  -- Pass color specific arguments
  newShader <- case inColorArray of
    PrivInColor'Hex colorArray' -> do
      pure $ newGradientColorHex (coercePtr colorArray')
    PrivInColor colorArray' colorspace -> do
      pure $ newGradientColor4f colorArray' (ptrOrNull colorspace)

  -- Pass other common arguments
  localMatrix' <- storable $ toSKMatrix localMatrix
  liftIO $ newShader (castPtr colorPosArray) (fromIntegral colorCount) (marshalSKEnum tileMode) localMatrix'

-- | Creates a gradient shader from a list of colors and positions.
createGradient ::
  (MonadResource m) =>
  Gradient ->
  -- | List of (color position, color hex)
  [(Float, SKColor)] ->
  SKShaderTileMode ->
  -- | Local matrix
  M33 Float ->
  m (ReleaseKey, SKShader)
createGradient gradient entries tileMode localMatrix =
  allocateSKObjectNeverNull
    ( evalManaged do
        let (posArray, hexArray) = unzip entries
        (posArray', numEntries) <- managed $ withArrayLen' posArray
        hexArray' <- managed $ withArray hexArray
        liftIO $ privCreateGradientRaw gradient (PrivInColor'Hex hexArray') posArray' numEntries tileMode localMatrix
    )
    sk_shader_unref

-- | Like 'createGradient' but uses RGBA colors instead of 'SKColor'.
createGradientRGBA ::
  (MonadResource m) =>
  Gradient ->
  -- | List of (color position, color rgba)
  [(Float, RGBA Float)] ->
  Maybe SKColorSpace ->
  SKShaderTileMode ->
  -- | Local matrix
  M33 Float ->
  m (ReleaseKey, SKShader)
createGradientRGBA gradient entries colorspace tileMode localMatrix =
  allocateSKObjectNeverNull
    ( evalManaged do
        let (posArray, colorArray) = unzip entries
        (posArray', numEntries) <- managed $ withArrayLen' posArray
        colorArray' <- managed $ withArray $ fmap toSKColor4f colorArray
        privCreateGradientRaw gradient (PrivInColor colorArray' colorspace) posArray' numEntries tileMode localMatrix
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
  m (ReleaseKey, SKShader)
createFractualNoise (V2 fx fy) numOctaves seed tileSize =
  allocateSKObjectNeverNull
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
  m (ReleaseKey, SKShader)
createTurbulence (V2 fx fy) numOctaves seed tileSize =
  allocateSKObjectNeverNull
    ( evalManaged do
        tileSize' <- useNullIfNothing storable $ toSKISize <$> tileSize
        liftIO $ sk_shader_new_perlin_noise_turbulence (coerce fx) (coerce fy) (fromIntegral numOctaves) (coerce seed) tileSize'
    )
    sk_shader_unref

{- | Return a shader that will apply the specified localMatrix to this shader.
The specified matrix will be applied before any matrix associated with this
shader.
-}
makeWithLocalMatrix :: (MonadResource m) => SKShader -> M33 Float -> m (ReleaseKey, SKShader)
makeWithLocalMatrix shader localMatrix =
  allocateSKObjectNeverNull
    ( evalManaged do
        localMatrix' <- storable $ toSKMatrix localMatrix
        liftIO $ sk_shader_with_local_matrix (ptr shader) localMatrix'
    )
    sk_shader_unref

{- | Create a new shader that produces the same colors as invoking this shader
and then applying the colorfilter.
-}
makeWithColorFilter :: (MonadResource m) => SKShader -> SKColorFilter -> m (ReleaseKey, SKShader)
makeWithColorFilter shader colorFilter =
  allocateSKObjectNeverNull
    ( evalManaged do
        liftIO $ sk_shader_with_color_filter (ptr shader) (ptr colorFilter)
    )
    sk_shader_unref
