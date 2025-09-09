module Skia.Skottie.AnimationBuilder where

import Data.ByteString qualified as BS
import Foreign.C.String qualified
import Skia.Skottie.Internal.Prelude
import Skia.SkResources.Objects as SkResources
import Language.C.Inline.Cpp qualified as C

C.context $ mconcat
  [ C.cppCtx
  , cppSkottieObjectTypes
  , cppSkResourcesObjectTypes
  , cppSkiaObjectTypes
  ]

C.include "modules/skottie/include/Skottie.h"

-- | Flags for 'create'.
data BuilderFlags = BuilderFlags
  { deferImageLoading :: Bool
  -- ^ Normally, all static image frames are resolved at load time via
  -- ImageAsset::getFrame(0).  With this flag, frames are only resolved when
  -- needed, at seek() time.
  , preferEmbeddedFonts :: Bool
  -- ^ Attempt to use the embedded fonts (glyph paths, normally used as
  -- fallback) over native Skia typefaces.
  }
  deriving (Show, Eq, Ord)

defaultBuilderFlags :: BuilderFlags
defaultBuilderFlags =
  BuilderFlags
    { deferImageLoading = False
    , preferEmbeddedFonts = False
    }

-- | Creates a new 'AnimationBuilder' instance.
create ::
  (MonadResource m) =>
  -- | Additional options. Consider using 'defaultBuilderFlags'.
  BuilderFlags ->
  m (ReleaseKey, AnimationBuilder)
create flags =
  allocateSkObjectNeverNull
    [C.exp| skottie::Animation::Builder* {
      new skottie::Animation::Builder($(uint32_t flags'))
    }|]
    (\p -> [C.block| void { delete $(skottie::Animation::Builder* p); } |])
  where
    flags' = 
      makeBitFlags
        [ (flags.deferImageLoading, [C.pure| uint32_t { skottie::Animation::Builder::Flags::kDeferImageLoading } |])
        , (flags.preferEmbeddedFonts, [C.pure| uint32_t { skottie::Animation::Builder::Flags::kPreferEmbeddedFonts } |])
        ]

data Stats = Stats
  { totalLoadTimeMS :: Float
  -- ^ Total animation instantiation time in milliseconds
  , jsonParseTimeMS :: Float
  -- ^ Time spent building a JSON DOM in milliseconds
  , sceneParseTimeMS :: Float
  -- ^ Time spent constructing the animation scene graph in milliseconds
  , jsonSize :: Int
  -- ^ Input JSON size in bytes
  , animatorCount :: Int
  -- ^ Number of dynamically animated properties
  }
  deriving (Show, Eq, Ord)

-- | Returns various animation build stats.
getStats :: (MonadIO m) => AnimationBuilder -> m Stats
getStats (ptr -> builder) = evalManaged do
  fTotalLoadTimeMS <- managed $ alloca @CFloat
  fJsonParseTimeMS <- managed $ alloca @CFloat
  fSceneParseTimeMS <- managed $ alloca @CFloat
  fJsonSize <- managed $ alloca @CSize
  fAnimatorCount <- managed $ alloca @CSize

  liftIO do
    [C.block| void {
      auto stats = $(skottie::Animation::Builder* builder)->getStats();
      *$(float* fTotalLoadTimeMS) = stats.fTotalLoadTimeMS;
      *$(float* fJsonParseTimeMS) = stats.fJsonParseTimeMS;
      *$(float* fSceneParseTimeMS) = stats.fSceneParseTimeMS;
      *$(size_t* fJsonSize) = stats.fJsonSize;
      *$(size_t* fAnimatorCount) = stats.fAnimatorCount;
    }|]

  totalLoadTimeMS <- liftIO $ coerce <$> peek fTotalLoadTimeMS
  jsonParseTimeMS <- liftIO $ coerce <$> peek fJsonParseTimeMS
  sceneParseTimeMS <- liftIO $ coerce <$> peek fSceneParseTimeMS
  jsonSize <- liftIO $ fromIntegral <$> peek fJsonSize
  animatorCount <- liftIO $ fromIntegral <$> peek fAnimatorCount

  pure Stats
    { totalLoadTimeMS
    , jsonParseTimeMS
    , sceneParseTimeMS
    , jsonSize
    , animatorCount
    }

-- | Specify a loader for external resources (images, etc.).
setResourceProvider :: (MonadIO m, SkResources.IsResourceProvider provider) => AnimationBuilder -> provider -> m ()
setResourceProvider (ptr -> builder) (ptr . toA SkResources.ResourceProvider -> provider) = liftIO do
  [C.block| void {
    $(skottie::Animation::Builder* builder)->setResourceProvider(
      sk_sp<skresources::ResourceProvider>($(skresources::ResourceProvider* provider)));
  }|]

-- | Specify a font manager for loading animation fonts.
setFontManager :: (MonadIO m) => AnimationBuilder -> SkFontMgr -> m ()
setFontManager (ptr -> builder) (ptr -> fontManager) = liftIO do
  [C.block| void {
    $(skottie::Animation::Builder* builder)->setFontManager(
      sk_sp<SkFontMgr>($(SkFontMgr* fontManager))
    );
  }|]

-- * Animation factories

-- | Builds an animation from a stream containing the JSON data.
buildFromStream ::
  (MonadResource m, IsSkStream stream) =>
  AnimationBuilder ->
  -- | The stream containing the JSON data.
  stream ->
  m (Maybe (ReleaseKey, Animation))
buildFromStream (ptr -> builder) (ptr . toA SkStream -> stream) =
  allocateSkObjectOrNothingIfNull
    [C.exp| skottie::Animation* {
      $(skottie::Animation::Builder* builder)->make($(SkStream* stream)).release()
    }|]
    (\p -> [C.block| void { delete $(skottie::Animation* p); } |])

-- | Builds an animation from a 'ByteString' containing the JSON data.
buildFromByteString ::
  (MonadResource m) =>
  AnimationBuilder ->
  -- | The JSON data.
  BS.ByteString ->
  m (Maybe (ReleaseKey, Animation))
buildFromByteString (ptr -> builder) bytestring =
    allocateSkObjectOrNothingIfNull
      ( evalManaged do
          (cstr, fromIntegral -> len) <- storableByteStringLen bytestring
          liftIO [C.exp| skottie::Animation* {
            $(skottie::Animation::Builder* builder)->make(
              $(const char* cstr),
              $(size_t len)
            ).release()
          }|]
      )
      (\p -> [C.block| void { delete $(skottie::Animation* p); } |])

-- | Builds an animation from a JSON file given its path.
buildFromFile ::
  (MonadResource m) =>
  AnimationBuilder ->
  -- | Path to the JSON file.
  FilePath ->
  m (Maybe (ReleaseKey, Animation))
buildFromFile (ptr -> builder) path =
  allocateSkObjectOrNothingIfNull
    ( evalManaged do
        path' <- managed $ Foreign.C.String.withCString path
        liftIO [C.exp| skottie::Animation* {
          $(skottie::Animation::Builder* builder)->makeFromFile($(const char* path')).release()
        }|]
    )
    (\p -> [C.block| void { delete $(skottie::Animation* p); } |])
