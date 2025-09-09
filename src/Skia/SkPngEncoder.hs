module Skia.SkPngEncoder where

import Data.Maybe
import Skia.Internal.Prelude
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "encode/SkPngEncoder.h"

data FilterFlags = FilterFlags
  { none :: Bool
  , sub :: Bool
  , up :: Bool
  , avg :: Bool
  , paeth :: Bool
  }
  deriving (Show, Eq, Ord)

data Options = Options
  { filterFlags :: FilterFlags
  , zlibLevel :: Int
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  -- NOTE: Taken from include/encode/SkPngEncoder.h
  Options
    { filterFlags = 
        FilterFlags
          { none = True
          , sub = True
          , up = True
          , avg = True
          , paeth = True
          }
    , zlibLevel = 6
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
    cflags = bitOrs $ mapMaybe
      (\(flagBool, flagBit) -> guard flagBool $> flagBit)
      [ (opts.filterFlags.none, [C.pure| int { (int) SkPngEncoder::FilterFlag::kNone } |])
      , (opts.filterFlags.sub, [C.pure| int { (int) SkPngEncoder::FilterFlag::kSub } |])
      , (opts.filterFlags.up, [C.pure| int { (int) SkPngEncoder::FilterFlag::kUp } |])
      , (opts.filterFlags.avg, [C.pure| int { (int) SkPngEncoder::FilterFlag::kAvg } |])
      , (opts.filterFlags.paeth, [C.pure| int { (int) SkPngEncoder::FilterFlag::kPaeth } |])
      ]

    czliblevel :: CInt = fromIntegral opts.zlibLevel

  liftIO $ toBool <$> [C.block|bool {
    SkPngEncoder::Options options;
    options.fFilterFlags = (SkPngEncoder::FilterFlag) $(int cflags);
    options.fZLibLevel = $(int czliblevel);
    options.fComments = nullptr;

    return SkPngEncoder::Encode($(SkWStream* dst'), *$(SkPixmap* pixmap), options);
  }|]


