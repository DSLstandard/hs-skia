{- | You may want to read \"Skia Color Management\"
(<https://skia.org/docs/user/color/>) to learn more.
-}
module Skia.SKColorSpace where

import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Linear
import Skia.Bindings.Sk_colorspace
import Skia.Bindings.Types
import Skia.Internal.Prelude
import Skia.SKRefCnt qualified as SKRefCnt
import System.IO.Unsafe

-- * Creating SKColorSpace

-- | Create the sRGB color space.
createSRGB :: (MonadResource m) => m (ReleaseKey, SKColorSpace)
createSRGB =
  allocateSKObjectNeverNull'
    sk_colorspace_new_srgb
    SKRefCnt.decrementNV

-- | Colorspace with the sRGB primaries, but a linear (1.0) gamma.
createSRGBLinear :: (MonadResource m) => m (ReleaseKey, SKColorSpace)
createSRGBLinear =
  allocateSKObjectNeverNull'
    sk_colorspace_new_srgb_linear
    SKRefCnt.decrementNV

{- | Create an SkColorSpace from a transfer function and a row-major 3x3
transformation to XYZ.
-}
createRGB ::
  (MonadResource m) =>
  TransferFn Float ->
  -- | Linear transform to XYZ D50.
  M33 Float ->
  m (ReleaseKey, SKColorSpace)
createRGB transferFn toXYZD50 =
  allocateSKObjectNeverNull'
    ( evalManaged do
        transferFn' <- storable $ marshalTransferFn transferFn
        toXYZD50' <- storable $ toSKColorSpaceXYZ toXYZD50
        liftIO $ sk_colorspace_new_rgb transferFn' toXYZD50'
    )
    SKRefCnt.decrementNV

-- | Creates an SkColorSpace from a parsed ICC profile.
createFromICCProfile :: (MonadResource m) => SKColorSpaceICCProfile -> m (ReleaseKey, SKColorSpace)
createFromICCProfile iccprof =
  allocateSKObjectNeverNull'
    (sk_colorspace_new_icc (ptr iccprof))
    SKRefCnt.decrementNV

{- | Returns a color space with the same gamut as this one, but with a linear
gamma.
-}
createLinearGamma :: (MonadResource m) => SKColorSpace -> m (ReleaseKey, SKColorSpace)
createLinearGamma colorspace =
  allocateSKObjectNeverNull'
    (sk_colorspace_make_linear_gamma (ptr colorspace))
    SKRefCnt.decrementNV

{- | Returns a color space with the same gamut as this one, but with the sRGB
transfer function.
-}
createSRGBGamma :: (MonadResource m) => SKColorSpace -> m (ReleaseKey, SKColorSpace)
createSRGBGamma colorspace =
  allocateSKObjectNeverNull'
    (sk_colorspace_make_srgb_gamma (ptr colorspace))
    SKRefCnt.decrementNV

-- * Getters

{- | Returns true if the color space gamma is near enough to be approximated as
sRGB.
-}
isGammaCloseToSRGB :: (MonadIO m) => SKColorSpace -> m Bool
isGammaCloseToSRGB space = liftIO do
  fmap toBool $ sk_colorspace_gamma_close_to_srgb (ptr space)

-- | Returns true if the color space gamma is linear.
isGammaLinear :: (MonadIO m) => SKColorSpace -> m Bool
isGammaLinear space = liftIO do
  fmap toBool $ sk_colorspace_gamma_is_linear (ptr space)

{- | Returns true if the color space is sRGB. Returns false otherwise.

This allows a little bit of tolerance, given that we might see small numerical
error in some cases: converting ICC fixed point to float, converting white point
to D50, rounding decisions on transfer function and matrix.

This does not consider a 2.2f exponential transfer function to be sRGB. While
these functions are similar (and it is sometimes useful to consider them
together), this function checks for logical equality.
-}
isSRGB :: (MonadIO m) => SKColorSpace -> m Bool
isSRGB space = liftIO do
  fmap toBool $ sk_colorspace_is_srgb (ptr space)

-- | Returns the to-XYZ-D50 linear transform of the 'SKColorSpace'.
getToXYZD50 :: (MonadIO m) => SKColorSpace -> m (M33 Float)
getToXYZD50 cs = evalManaged do
  xyz' <- managed alloca

  -- NOTE: _result is ALWAYS true. See Google Skia's
  -- include/core/SkColorSpace.h's toXYZD50 in class SkColorSpace.
  _result <- liftIO $ sk_colorspace_to_xyzd50 (ptr cs) xyz'

  peekWith fromSKColorSpaceXYZ xyz'

{- | Returns the 'TransferFn' of the 'SKColorSpace' if the transfer function can
be represented as coefficients to the standard ICC 7-parameter equation. Returns
'Nothing' otherwise (eg, PQ, HLG).
-}
asNumericalTransferFn :: (MonadIO m) => SKColorSpace -> m (Maybe (TransferFn Float))
asNumericalTransferFn cs = evalManaged do
  fn' <- managed alloca
  ok <- liftIO $ fmap toBool $ sk_colorspace_is_numerical_transfer_fn (ptr cs) fn'
  if ok
    then do
      Just <$> peekWith unmarshalTransferFn fn'
    else do
      pure Nothing

-- | Returns true if two colorspaces are equivalent.
checkAreEqual :: (MonadIO m) => SKColorSpace -> SKColorSpace -> m Bool
checkAreEqual cs1 cs2 = liftIO do
  fmap toBool $ sk_colorspace_equals (ptr cs1) (ptr cs2)

-- | Converts this color space to an skcms ICC profile struct.
getICCProfile :: (MonadResource m) => SKColorSpace -> m (ReleaseKey, SKColorSpaceICCProfile)
getICCProfile cs = do
  (key, profile') <- allocate sk_colorspace_icc_profile_new sk_colorspace_icc_profile_delete
  liftIO $ sk_colorspace_to_profile (ptr cs) profile'
  pure (key, fromPtr profile')

-- * ICCProfile utils

{- | Creates an 'SKColorSpaceICCProfile' by parsing an ICC profile.

Throws a 'SkiaError' if the operation fails.

Note that this function always sets the @toXYZD50@ of the returned
'SKColorSpaceICCProfile', so 'getICCProfileToXYZD50' should always return
'Just'.

For reference, Google Skia comments: \"for continuity of existing user
expectations, prefer A2B0 (perceptual) over A2B1 (relative colormetric), and
ignore A2B2 (saturation).\"
-}
parseICCProfile ::
  (MonadResource m) =>
  -- | Input buffer containing an ICC profile.
  BS.ByteString ->
  m (ReleaseKey, SKColorSpaceICCProfile)
parseICCProfile source = do
  (releaseKey, profile') <- allocate sk_colorspace_icc_profile_new sk_colorspace_icc_profile_delete
  ok <- liftIO $
    BS.unsafeUseAsCStringLen source \(cstr, len) -> do
      sk_colorspace_icc_profile_parse (castPtr cstr) (fromIntegral len) profile'
  unless (toBool ok) do
    liftIO $ throwIO $ SkiaError "Cannot parse ICC profile from given data"
  pure (releaseKey, fromPtr profile')

{- | O(1). Exposes the internal buffer of a 'SKColorSpaceICCProfile'.

You must not modify the contents of the buffer.
-}
unsafeWithICCProfileBuffer ::
  SKColorSpaceICCProfile ->
  (CStringLen -> IO r) ->
  IO r
unsafeWithICCProfileBuffer profile f = evalManaged do
  len' <- managed alloca
  cstr <- liftIO $ sk_colorspace_icc_profile_get_buffer (ptr profile) len'
  len <- peekWith fromIntegral len'
  liftIO $ f (castPtr cstr, len)

{- | If this profile's gamut can be represented by a 3x3 transform to XYZD50,
returns 'Just' along with the matrix, otherwise 'Nothing'.
-}
getICCProfileToXYZD50 :: (MonadIO m) => SKColorSpaceICCProfile -> m (Maybe (M33 Float))
getICCProfileToXYZD50 profile = evalManaged do
  xyz' <- managed alloca
  exists <- liftIO $ fmap toBool $ sk_colorspace_icc_profile_get_to_xyzd50 (ptr profile) xyz'
  if exists
    then do
      Just <$> peekWith fromSKColorSpaceXYZ xyz'
    else do
      pure Nothing

-- * Transfer function utils

{- | A transfer function mapping encoded values to linear values, represented by
this 7-parameter piecewise function:

@ linear = sign(encoded) *  (c*|encoded| + f)       , 0 <= |encoded| < d =
sign(encoded) * ((a*|encoded| + b)^g + e), d <= |encoded| @

(A simple gamma transfer function sets g to gamma and a to 1.)

Parameters have the generic type @t@, though Google Skia operates on 'Float's.
-}
data TransferFn t = TransferFn
  { g :: t
  , a :: t
  , b :: t
  , c :: t
  , d :: t
  , e :: t
  , f :: t
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

marshalTransferFn :: TransferFn Float -> Sk_colorspace_transfer_fn
marshalTransferFn fn = Sk_colorspace_transfer_fn{fG = coerce fn.g, fA = coerce fn.a, fB = coerce fn.b, fC = coerce fn.c, fD = coerce fn.d, fE = coerce fn.e, fF = coerce fn.f}

unmarshalTransferFn :: Sk_colorspace_transfer_fn -> TransferFn Float
unmarshalTransferFn fn = TransferFn{g = coerce fn.fG, a = coerce fn.fA, b = coerce fn.fB, c = coerce fn.fC, d = coerce fn.fD, e = coerce fn.fE, f = coerce fn.fF}

{- | A list of colorspace transfer functions important enough for Google Skia to
name them.

You can use 'namedTransferFn' to get the corresponding 'TransferFn'.
-}
data NamedTransferFn
  = NamedTransferFn'SRGB
  | NamedTransferFn'2Dot2
  | NamedTransferFn'Linear
  | NamedTransferFn'Rec2020
  | NamedTransferFn'PQ
  | NamedTransferFn'HLG
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Resolves a 'NamedTransferFn' to a 'TransferFn'.
namedTransferFn :: (Fractional a) => NamedTransferFn -> TransferFn a
namedTransferFn = \case
  -- DEVNOTE: See include/core/SkColorSpace.h for transferFn'* definitions
  NamedTransferFn'SRGB -> TransferFn{g = 2.4, a = 1 / 1.055, b = 0.055 / 1.055, c = 1 / 12.92, d = 0.04045, e = 0.0, f = 0.0}
  NamedTransferFn'2Dot2 -> TransferFn{g = 2.2, a = 1.0, b = 0.0, c = 0.0, d = 0.0, e = 0.0, f = 0.0}
  NamedTransferFn'Linear -> TransferFn{g = 1.0, a = 1.0, b = 0.0, c = 0.0, d = 0.0, e = 0.0, f = 0.0}
  NamedTransferFn'Rec2020 -> TransferFn{g = 2.22222, a = 0.909672, b = 0.0903276, c = 0.222222, d = 0.0812429, e = 0.0, f = 0.0}
  NamedTransferFn'PQ -> TransferFn{g = -2.0, a = -107 / 128.0, b = 1.0, c = 32 / 2523.0, d = 2413 / 128.0, e = -2392 / 128.0, f = 8192 / 1305.0}
  NamedTransferFn'HLG -> TransferFn{g = -3.0, a = 2.0, b = 2.0, c = 1 / 0.17883277, d = 0.28466892, e = 0.55991073, f = 0.0}

-- * XYZD50 utils

{- | A list of toXYZD50 linear transforms important enough for Google Skia to
name them.

You can use 'namedToXYZD50' to get the corresponding 'M33' transform.
-}
data NamedToXYZD50
  = NamedToXYZD50'SRGB
  | NamedToXYZD50'AdobeRGB
  | NamedToXYZD50'DisplayP3
  | NamedToXYZD50'Rec2020
  | NamedToXYZD50'XYZ
  deriving (Show, Eq, Ord, Enum, Bounded)

namedToXYZD50 :: forall a. (Fractional a) => NamedToXYZD50 -> M33 a
namedToXYZD50 = \case
  -- See include/core/SkColorSpace.h
  NamedToXYZD50'SRGB ->
    V3
      (V3 0.436065674 0.385147095 0.143066406)
      (V3 0.222488403 0.716873169 0.060607910)
      (V3 0.013916016 0.097076416 0.714096069)
  NamedToXYZD50'AdobeRGB ->
    V3
      (V3 0.60974 0.20528 0.14919)
      (V3 0.31111 0.62567 0.06322)
      (V3 0.01947 0.06087 0.74457)
  NamedToXYZD50'DisplayP3 ->
    V3
      (V3 0.515102 0.291965 0.157153)
      (V3 0.241182 0.692236 0.0665819)
      (V3 (-0.00104941) 0.0418818 0.784378)
  NamedToXYZD50'Rec2020 ->
    V3
      (V3 0.673459 0.165661 0.125100)
      (V3 0.279033 0.675338 0.0456288)
      (V3 (-0.00193139) 0.0299794 0.797162)
  NamedToXYZD50'XYZ ->
    identity

-- * Color primaries utils

{- | Describes a color gamut with primaries and a white point.

Parameters have the generic type @t@, though Google Skia operates on 'Float's.
-}
data Primaries a = Primaries
  { red :: V2 a
  , green :: V2 a
  , blue :: V2 a
  , white :: V2 a
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

marshalPrimaries :: Primaries Float -> Sk_colorspace_primaries
marshalPrimaries
  ( coerce ->
      Primaries
        { red = V2 fRX fRY
        , green = V2 fGX fGY
        , blue = V2 fBX fBY
        , white = V2 fWX fWY
        }
    ) =
    Sk_colorspace_primaries{..}

{- | Returns a matrix to convert RGB color into XYZ adapted to D50, given the
primaries and whitepoint of the RGB model.

Returns 'Nothing' if a toXYZD50 matrix cannot be properly constructed from the
given primaries.
-}
primariesToXYZD50 :: Primaries Float -> Maybe (M33 Float)
primariesToXYZD50 primaries = unsafeDupablePerformIO $ evalManaged do
  -- TODO: Would it be a good idea to convert the code from
  -- modules/skcms/skcms.cc:skcms_PrimariesToXYZD50 into pure-Haskell to allow
  -- generic 'Fractional'/'Real' types?
  primaries' <- storable $ marshalPrimaries primaries
  xyz' <- managed alloca
  ok <- liftIO $ fmap toBool $ sk_colorspace_primaries_to_xyzd50 primaries' xyz'
  if ok
    then do
      Just <$> peekWith fromSKColorSpaceXYZ xyz'
    else do
      pure Nothing
{-# NOINLINE primariesToXYZD50 #-}

-- * Linear conversion utils

toSKColorSpaceXYZ :: M33 Float -> Sk_colorspace_xyz
toSKColorSpaceXYZ
  ( coerce ->
      V3
        (V3 fM00 fM01 fM02)
        (V3 fM10 fM11 fM12)
        (V3 fM20 fM21 fM22)
    ) =
    Sk_colorspace_xyz{..}

fromSKColorSpaceXYZ :: Sk_colorspace_xyz -> M33 Float
fromSKColorSpaceXYZ Sk_colorspace_xyz{..} =
  coerce $
    V3
      (V3 fM00 fM01 fM02)
      (V3 fM10 fM11 fM12)
      (V3 fM20 fM21 fM22)
