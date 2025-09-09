module Skia.SkSurfaceProps where

import Language.C.Inline.Cpp qualified as C
import NeatInterpolation
import Skia.Internal.Prelude
import Skia.Internal.THUtils

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkSurfaceProps.h"

-- About the definition of 'SkSurfaceProps', see Google Skia's
-- include/core/SkSurfaceProps.h

$( qGenerateSkEnum
  "SKPixelGeometry"
  [trimming|
      Description of how the LCD strips are arranged for each pixel. If this is unknown, or the
      pixels are meant to be "portable" and/or transformed before showing (e.g. rotated, scaled)
      then use 'SKPixelGeometry'Unknown'.
  |]
  [ ("Unknown", "SkPixelGeometry::kUnknown_SkPixelGeometry", "")
  , ("RGB'H", "SkPixelGeometry::kRGB_H_SkPixelGeometry", "")
  , ("BGR'H", "SkPixelGeometry::kBGR_H_SkPixelGeometry", "")
  , ("RGB'V", "SkPixelGeometry::kRGB_V_SkPixelGeometry", "")
  , ("BGR'V", "SkPixelGeometry::kBGR_V_SkPixelGeometry", "")
  ]
 )


{- | Describes properties and constraints of a given SkSurface. The rendering
engine can parse these during drawing, and can sometimes optimize its
performance (e.g. disabling an expensive feature).
-}
data SurfaceProps = SurfaceProps
  { usesDeviceIndependentFonts :: Bool
  , usesDynamicMSAA :: Bool
  -- ^ Use internal MSAA to render to non-MSAA GPU surfaces.
  , alwaysDither :: Bool
  -- ^ If set, all rendering will have dithering enabled. Currently this only
  -- impacts GPU backends
  , pixelGeometry :: SKPixelGeometry
  }
  deriving (Show, Eq, Ord)

-- * Internal utils

marshalSkSurfaceProps :: SurfaceProps -> Managed (Ptr C'SkSurfaceProps)
marshalSkSurfaceProps SurfaceProps{..} = do
  managed $
    Control.Exception.bracket
      [C.block|SkSurfaceProps* {
        uint32_t flags = SkSurfaceProps::Flags::kDefault_Flag;
        if ($(bool cusesDeviceIndependentFonts)) {
          flags |= SkSurfaceProps::Flags::kUseDeviceIndependentFonts_Flag;
        }
        if ($(bool cusesDynamicMSAA)) {
          flags |= SkSurfaceProps::Flags::kDynamicMSAA_Flag;
        }
        if ($(bool calwaysDither)) {
          flags |= SkSurfaceProps::Flags::kAlwaysDither_Flag;
        }
        return new SkSurfaceProps(flags, (SkPixelGeometry) $(int cpixelgeometry));
      }|]
      (\p -> [C.block|void { delete $(SkSurfaceProps* p); }|])
 where
  cpixelgeometry = marshalSkEnum pixelGeometry
  cusesDeviceIndependentFonts :: CBool = fromBool usesDeviceIndependentFonts
  cusesDynamicMSAA :: CBool = fromBool usesDynamicMSAA
  calwaysDither :: CBool = fromBool alwaysDither

peekSkSurfaceProps :: (MonadIO m) => Ptr C'SkSurfaceProps -> m SurfaceProps
peekSkSurfaceProps props' = evalManaged do
  usesDeviceIndependentFonts' <- managed $ alloca @CBool
  usesDynamicMSAA' <- managed $ alloca @CBool
  alwaysDither' <- managed $ alloca @CBool
  pixelGeometry' <- managed $ alloca @CInt

  liftIO [C.block|void {
    SkSurfaceProps* p = $(SkSurfaceProps* props');

    uint32_t flags = p->flags();
    *$(bool* usesDeviceIndependentFonts') = flags & SkSurfaceProps::Flags::kUseDeviceIndependentFonts_Flag;
    *$(bool* usesDynamicMSAA') = flags & SkSurfaceProps::Flags::kDynamicMSAA_Flag;
    *$(bool* alwaysDither') = flags & SkSurfaceProps::Flags::kAlwaysDither_Flag;

    SkPixelGeometry geo = p->pixelGeometry();
    *$(int* pixelGeometry') = (int) geo;
  }|]

  usesDeviceIndependentFonts <- liftIO $ peek usesDeviceIndependentFonts'
  usesDynamicMSAA <- liftIO $ peek usesDynamicMSAA'
  alwaysDither <- liftIO $ peek alwaysDither'
  pixelGeometry <- liftIO $ unmarshalSkEnumOrDie =<< peek pixelGeometry'

  pure SurfaceProps
    { usesDeviceIndependentFonts = toBool usesDeviceIndependentFonts
    , usesDynamicMSAA = toBool usesDynamicMSAA
    , alwaysDither = toBool alwaysDither
    , pixelGeometry
    }
