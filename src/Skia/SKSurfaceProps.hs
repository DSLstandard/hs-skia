module Skia.SKSurfaceProps where

import Skia.Bindings.Sk_surface
import Skia.Bindings.Types
import Skia.Internal.Prelude

-- About the definition of 'SKSurfaceProps', see Google Skia's
-- include/core/SkSurfaceProps.h

{- | Describes properties and constraints of a given SKSurface. The rendering
engine can parse these during drawing, and can sometimes optimize its
performance (e.g. disabling an expensive feature).
-}
data SKSurfaceProps = SKSurfaceProps
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

peekSKSurfaceProps :: (MonadIO m) => Ptr Sk_surfaceprops -> m SKSurfaceProps
peekSKSurfaceProps props' = liftIO do
  flags <- Sk_surfaceprops_flags <$> sk_surfaceprops_get_flags props'

  let usesDeviceIndependentFonts = hasFlag USE_DEVICE_INDEPENDENT_FONTS_SK_SURFACE_PROPS_FLAGS flags
  let usesDynamicMSAA = hasFlag DYNAMIC_MSAA_SK_SURFACE_PROPS_FLAGS flags
  let alwaysDither = hasFlag ALWAYS_DITHER_SK_SURFACE_PROPS_FLAGS flags

  pixelGeometry <- unmarshalSKEnumOrDie =<< sk_surfaceprops_get_pixel_geometry props'
  pure SKSurfaceProps{..}

useSKSurfaceProps :: SKSurfaceProps -> Managed (Ptr Sk_surfaceprops)
useSKSurfaceProps props =
  managed $
    Control.Exception.bracket
      (sk_surfaceprops_new flags pixelGeometry)
      sk_surfaceprops_delete
 where
  pixelGeometry = marshalSKEnum props.pixelGeometry

  Sk_surfaceprops_flags flags =
    makeBitFlags
      [ (props.usesDeviceIndependentFonts, USE_DEVICE_INDEPENDENT_FONTS_SK_SURFACE_PROPS_FLAGS)
      , (props.usesDynamicMSAA, DYNAMIC_MSAA_SK_SURFACE_PROPS_FLAGS)
      , (props.alwaysDither, ALWAYS_DITHER_SK_SURFACE_PROPS_FLAGS)
      ]

-- | Used to allocate an empty 'Sk_surfaceprops' as a destination buffer.
useAllocaSKSurfaceProps :: Managed (Ptr Sk_surfaceprops)
useAllocaSKSurfaceProps =
  managed $
    Control.Exception.bracket
      (sk_surfaceprops_new 0 0) -- Bogus values.
      sk_surfaceprops_delete
