module Skia.SKParagraph.StrutStyle where

import Data.Coerce
import Data.Text qualified as T
import Data.Traversable
import Skia.Bindings.Skparagraph
import Skia.Bindings.Types
import Skia.SKFontStyle
import Skia.SKParagraph.Internal.Prelude
import Skia.SKString as SKString

-- | Creates a new empty 'StrutStyle' object.
createEmpty :: (MonadResource m) => m (ReleaseKey, StrutStyle)
createEmpty =
  allocateSKObjectNeverNull
    skparagraph_strut_style_new
    skparagraph_strut_style_delete

-- | Sets the font families for the strut style.
setFontFamilies :: (MonadIO m) => StrutStyle -> [T.Text] -> m ()
setFontFamilies strut families = liftIO $ runResourceT do
  -- Allocate Sk_string objects for each family name
  families' :: [Ptr Sk_string] <- for families \family -> do
    (_, familyPtr) <- SKString.createFromText family
    pure (ptr familyPtr)

  -- Pass the array of pointers to the C function
  liftIO $ Foreign.Marshal.Array.withArrayLen families' \len familiesPtr ->
    skparagraph_strut_style_set_font_families (ptr strut) familiesPtr (fromIntegral len)

-- | Sets the font style for the strut style.
setFontStyle :: (MonadIO m) => StrutStyle -> SKFontStyle -> m ()
setFontStyle strut style = evalManaged do
  style' <- marshalSKFontStyle style
  liftIO $ skparagraph_strut_style_set_font_style (ptr strut) style'

-- | Sets the font size for the strut style.
setFontSize :: (MonadIO m) => StrutStyle -> Float -> m ()
setFontSize strut size = liftIO do
  skparagraph_strut_style_set_font_size (ptr strut) (coerce size)

-- | Sets the height for the strut style.
setHeight :: (MonadIO m) => StrutStyle -> Float -> m ()
setHeight strut height = liftIO do
  skparagraph_strut_style_set_height (ptr strut) (coerce height)

-- | Sets the leading for the strut style.
setLeading :: (MonadIO m) => StrutStyle -> Float -> m ()
setLeading strut leading = liftIO do
  skparagraph_strut_style_set_leading (ptr strut) (coerce leading)

-- | Enables or disables the strut.
setStrutEnabled :: (MonadIO m) => StrutStyle -> Bool -> m ()
setStrutEnabled strut v = liftIO do
  skparagraph_strut_style_set_strut_enabled (ptr strut) (fromBool v)

-- | Forces the strut height.
setForceStrutHeight :: (MonadIO m) => StrutStyle -> Bool -> m ()
setForceStrutHeight strut v = liftIO do
  skparagraph_strut_style_set_force_strut_height (ptr strut) (fromBool v)

-- | Overrides the height.
setHeightOverride :: (MonadIO m) => StrutStyle -> Bool -> m ()
setHeightOverride strut v = liftIO do
  skparagraph_strut_style_set_height_override (ptr strut) (fromBool v)

-- | Sets half leading for the strut.
setHalfLeading :: (MonadIO m) => StrutStyle -> Bool -> m ()
setHalfLeading strut halfLeading = liftIO do
  skparagraph_strut_style_set_half_leading (ptr strut) (fromBool halfLeading)
