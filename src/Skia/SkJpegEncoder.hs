module Skia.SkJpegEncoder where

import Skia.Internal.Prelude
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "encode/SkJpegEncoder.h"

data AlphaOption
  = AlphaOption'Ignore
  | AlphaOption'BlendOnBlack
  deriving (Show, Eq, Ord)

data Downsample
  = Downsample'420 -- ^ Reduction by a factor of two in both the horizontal and vertical directions.
  | Downsample'422 -- ^ Reduction by a factor of two in the horizontal direction.
  | Downsample'444 -- ^ No downsampling.
  deriving (Show, Eq, Ord)

data Options = Options
  { quality :: Int
  , downsample :: Downsample
  , alphaOption :: AlphaOption
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  -- NOTE: Taken from include/encode/SkPngEncoder.h
  Options
    { quality = 100
    , downsample = Downsample'420
    , alphaOption = AlphaOption'Ignore
    }

encodePixmap ::
  (MonadIO m, IsSkWStream stream) =>
  -- | Destination stream
  stream ->
  -- | Source pixmap
  SkPixmap ->
  -- | Encoding options
  Options ->
  -- | Returns true on success. Returns false on an invalid or unsupported
  -- @src@.
  m Bool
encodePixmap (ptr . toA SkWStream -> dst') (ptr -> pixmap) opts = evalManaged do
  let
    cquality :: CInt = fromIntegral opts.quality

    cdownsample :: CInt = case opts.downsample of
      Downsample'420 -> [C.pure| int { (int) SkJpegEncoder::Downsample::k420 } |]
      Downsample'422 -> [C.pure| int { (int) SkJpegEncoder::Downsample::k422 } |]
      Downsample'444 -> [C.pure| int { (int) SkJpegEncoder::Downsample::k444 } |]

    calphaOption :: CInt = case opts.alphaOption of
      AlphaOption'Ignore -> [C.pure| int { (int) SkJpegEncoder::AlphaOption::kIgnore } |]
      AlphaOption'BlendOnBlack -> [C.pure| int { (int) SkJpegEncoder::AlphaOption::kBlendOnBlack } |]

  liftIO $ toBool <$> [C.block|bool {
    SkJpegEncoder::Options options;
    options.fQuality = $(int cquality);
    options.fDownsample = (SkJpegEncoder::Downsample) $(int cdownsample);
    options.fAlphaOption = (SkJpegEncoder::AlphaOption) $(int calphaOption);

    return SkJpegEncoder::Encode($(SkWStream* dst'), *$(SkPixmap* pixmap), options);
  }|]