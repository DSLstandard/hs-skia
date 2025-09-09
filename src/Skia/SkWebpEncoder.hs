module Skia.SkWebpEncoder where

import Skia.Internal.Prelude
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "encode/SkWebpEncoder.h"

data Compression = Compression'Lossy | Compression'Lossless
  deriving (Show, Eq, Ord, Bounded, Enum)

data Options = Options
  { compression :: Compression
  -- ^ Determines whether we will use webp lossy or lossless compression.
  , quality :: Float
  -- ^ Quality must be in @[0.0, 100.0]@.
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { compression = Compression'Lossy
    , quality = 100.0
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
    ccompression :: CInt = case opts.compression of
      Compression'Lossy -> [C.pure| int { (int) SkWebpEncoder::Compression::kLossy } |]
      Compression'Lossless -> [C.pure| int { (int) SkWebpEncoder::Compression::kLossless } |]

    cquality :: CFloat = coerce opts.quality

  liftIO $ toBool <$> [C.block|bool {
    SkWebpEncoder::Options options;
    options.fCompression = (SkWebpEncoder::Compression) $(int ccompression);
    options.fQuality = $(float cquality);

    return SkWebpEncoder::Encode($(SkWStream* dst'), *$(SkPixmap* pixmap), options);
  }|]

