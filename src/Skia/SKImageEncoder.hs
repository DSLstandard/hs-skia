module Skia.SKImageEncoder where

import Data.Maybe
import Data.Text qualified as T
import Skia.Bindings.Sk_pixmap
import Skia.Bindings.Types
import Skia.Internal.Prelude

-- * Core definitions and utilities

data EncoderOptions
  = EncoderOptions'Jpeg SKJpegEncoderOptions
  | EncoderOptions'Png SKPngEncoderOptions
  | EncoderOptions'Webp SKWebpEncoderOptions
  deriving (Show)

-- Encode the |src| pixels to the |dst| stream. |options| may be used to control
-- the encoding behavior.
--
-- Returns true on success.  Returns false on an invalid or unsupported |src|.
encodePixmap ::
  (IsSKWStream stream, MonadIO m) =>
  -- | Destination stream
  stream ->
  -- | Source pixmap
  SKPixmap ->
  EncoderOptions ->
  m Bool
encodePixmap (toA SKWStream -> stream) pixmap opts = evalManaged do
  success <- case opts of
    EncoderOptions'Jpeg opts -> do
      opts' <- marshalSKJpegEncoderOptions opts
      liftIO $ sk_jpegencoder_encode (ptr stream) (ptr pixmap) opts'
    EncoderOptions'Png opts -> do
      opts' <- marshalSKPngEncoderOptions opts
      liftIO $ sk_pngencoder_encode (ptr stream) (ptr pixmap) opts'
    EncoderOptions'Webp opts -> do
      opts' <- marshalSKWebpEncoderOptions opts
      liftIO $ sk_webpencoder_encode (ptr stream) (ptr pixmap) opts'
  pure $ toBool success

-- * WebP

data SKWebpEncoderOptions = SKWebpEncoderOptions
  { compression :: SKWebpEncoderCompression
  , quality :: Float
  , iccProfile :: Maybe SKColorSpaceICCProfile
  , iccProfileDescription :: Maybe T.Text
  }
  deriving (Show)

defaultSKWebpEncoderOptions :: SKWebpEncoderOptions
defaultSKWebpEncoderOptions =
  SKWebpEncoderOptions
    { compression = SKWebpEncoderCompression'Lossy
    , quality = 100.0
    , iccProfile = Nothing
    , iccProfileDescription = Nothing
    }

-- * JPEG

data SKJpegEncoderOptions = SKJpegEncoderOptions
  { quality :: Int
  , downsample :: SKJpegEncoderDownsample
  , alphaOption :: SKJpegEncoderAlphaOption
  , xmpMetadata :: Maybe SKData
  , iccProfile :: Maybe SKColorSpaceICCProfile
  , iccProfileDescription :: Maybe T.Text
  }
  deriving (Show)

defaultSKJpegEncoderOptions :: SKJpegEncoderOptions
defaultSKJpegEncoderOptions =
  SKJpegEncoderOptions
    { quality = 100
    , downsample = SKJpegEncoderDownsample'Downsample420
    , alphaOption = SKJpegEncoderAlphaOption'Ignore
    , xmpMetadata = Nothing
    , iccProfile = Nothing
    , iccProfileDescription = Nothing
    }

-- * PNG

data SKPngEncoderFilterFlags = SKPngEncoderFilterFlags
  { none :: Bool
  , sub :: Bool
  , up :: Bool
  , avg :: Bool
  , paeth :: Bool
  }
  deriving (Show, Eq, Ord)

data SKPngEncoderOptions = SKPngEncoderOptions
  { filterFlags :: SKPngEncoderFilterFlags
  , zlibLevel :: Int
  , comments :: Maybe (Ptr ())
  -- ^ TODO: What does this do?
  , iccProfile :: Maybe SKColorSpaceICCProfile
  , iccProfileDescription :: Maybe T.Text
  }
  deriving (Show)

defaultSKPngEncoderOptions :: SKPngEncoderOptions
defaultSKPngEncoderOptions =
  -- NOTE: Taken from include/encode/SkPngEncoder.h
  SKPngEncoderOptions
    { filterFlags
    , zlibLevel = 6
    , comments = Nothing
    , iccProfile = Nothing
    , iccProfileDescription = Nothing
    }
 where
  filterFlags =
    SKPngEncoderFilterFlags
      { none = True
      , sub = True
      , up = True
      , avg = True
      , paeth = True
      }

-- * Marshal utils

marshalSKPngEncoderOptions :: SKPngEncoderOptions -> Managed (Ptr Sk_pngencoder_options)
marshalSKPngEncoderOptions opts = do
  iccProfileDescription' <- useNullIfNothing storableTextUTF8NullTerminated opts.iccProfileDescription
  storable $
    Sk_pngencoder_options
      { fFilterFlags = marshalSKPngEncoderFilterFlags opts.filterFlags
      , fZLibLevel = fromIntegral opts.zlibLevel
      , fComments = fromMaybe nullPtr opts.comments
      , fICCProfile = ptrOrNull opts.iccProfile
      , fICCProfileDescription = iccProfileDescription'
      }
 where
  marshalSKPngEncoderFilterFlags :: SKPngEncoderFilterFlags -> Sk_pngencoder_filterflags
  marshalSKPngEncoderFilterFlags flags =
    bitOrs $
      mapMaybe
        (\(flagBool, flagBit) -> guard flagBool $> flagBit)
        [ (flags.none, NONE_SK_PNGENCODER_FILTER_FLAGS)
        , (flags.sub, SUB_SK_PNGENCODER_FILTER_FLAGS)
        , (flags.up, UP_SK_PNGENCODER_FILTER_FLAGS)
        , (flags.avg, AVG_SK_PNGENCODER_FILTER_FLAGS)
        , (flags.paeth, PAETH_SK_PNGENCODER_FILTER_FLAGS)
        ]

marshalSKJpegEncoderOptions :: SKJpegEncoderOptions -> Managed (Ptr Sk_jpegencoder_options)
marshalSKJpegEncoderOptions opts = do
  iccProfileDescription' <- useNullIfNothing storableTextUTF8NullTerminated opts.iccProfileDescription
  storable $
    Sk_jpegencoder_options
      { fQuality = fromIntegral opts.quality
      , fDownsample = marshalSKEnum opts.downsample
      , fAlphaOption = marshalSKEnum opts.alphaOption
      , xmpMetadata = ptrOrNull opts.xmpMetadata
      , fICCProfile = ptrOrNull opts.iccProfile
      , fICCProfileDescription = iccProfileDescription'
      }

marshalSKWebpEncoderOptions :: SKWebpEncoderOptions -> Managed (Ptr Sk_webpencoder_options)
marshalSKWebpEncoderOptions opts = evalManaged do
  iccProfileDescription' <- useNullIfNothing storableTextUTF8NullTerminated opts.iccProfileDescription
  storable $
    Sk_webpencoder_options
      { fCompression = marshalSKEnum opts.compression
      , fQuality = coerce opts.quality
      , fICCProfile = ptrOrNull opts.iccProfile
      , fICCProfileDescription = iccProfileDescription'
      }
