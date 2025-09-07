module Skia.SkFontMgr where

import Skia.Internal.Prelude
import Data.Text qualified as T
import Language.C.Inline.Cpp qualified as C
import Language.C.Inline.Internal qualified as C
import Skia.SkFontStyle
import Skia.SkRefCnt qualified as SkRefCnt
import NeatInterpolation

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkFontMgr.h"
C.include "core/SkTypeface.h"

-- NOTE: Using weak symbols as a trick to allow this .cpp to be compiled with
-- unsolved ::SkFontMgr_New_[XYZ]() references if Skia is not compiled to
-- support the corresponding SkFontMgr backend.

$(C.emitVerbatim $
  T.unpack [text|
    // ** include/ports/SkFontMgr_fontconfig.h

    // Copied from
    // https://github.com/servo/libfontconfig/blob/4e97940af72c90191594a688b21c381105aadca4/fontconfig/fontconfig.h#L289C1-L289C38
    typedef struct _FcConfig FcConfig;

    /** Create a font manager around a FontConfig instance.
    *  If 'fc' is NULL, will use a new default config.
    *  Takes ownership of 'fc' and will call FcConfigDestroy on it.
    */
    __attribute__((weak)) sk_sp<SkFontMgr> SkFontMgr_New_FontConfig(FcConfig* fc);

    // ** include/ports/SkTypeface_win.h

    __attribute__((weak)) sk_sp<SkFontMgr> SkFontMgr_New_GDI();
  |]
 )

-- * Creating SKFontManager

createByFontconfig :: (MonadResource m) => m (ReleaseKey, SkFontMgr)
createByFontconfig = do
  allocateSkObjectNeverNull'
    ( do
        unlessM isFontconfigAvailable do
          throwIO $ SkiaError "Fontconfig SkFontMgr backend is not available"
        [C.exp| SkFontMgr* { SkFontMgr_New_FontConfig(nullptr).release() }|]
    )
    SkRefCnt.decrement

createByGDI :: (MonadResource m) => m (ReleaseKey, SkFontMgr)
createByGDI = do
  allocateSkObjectNeverNull'
    ( do
        unlessM isGDIAvailable do
          throwIO $ SkiaError "GDI SkFontMgr backend is not available"
        [C.exp| SkFontMgr* { SkFontMgr_New_GDI().release() }|]
    )
    SkRefCnt.decrement

-- * Checking if a particular backend is supported

isFontconfigAvailable :: (MonadIO m) => m Bool
isFontconfigAvailable = liftIO do
  toBool <$> [C.exp| bool { SkFontMgr_New_FontConfig != nullptr }|]

isGDIAvailable :: (MonadIO m) => m Bool
isGDIAvailable = liftIO do
  toBool <$> [C.exp| bool { SkFontMgr_New_GDI != nullptr }|]

-- * Utils

{- | Finds the closest matching typeface to the specified family anme and style
and returns it. Will return 'Nothing' if no \'good\' match is found.

Passing 'Nothing' as the family name will return the default system font.

It is possible that this will return a style set not accessible from
"createStyleSet' or 'matchFamily' due to hidden or auto-activated fonts.
-}
matchFamilyStyle ::
  (MonadResource m) =>
  SkFontMgr ->
  -- | Family name
  Maybe T.Text ->
  FontStyle ->
  m (Maybe (ReleaseKey, SkTypeface))
matchFamilyStyle (ptr -> fmgr) familyName fontStyle =
  allocateSkObjectOrNothingIfNull'
    ( evalManaged do
        fontStyle' <- marshalSkFontStyle fontStyle
        familyName' <- maybe (pure nullPtr) storableTextUTF8NullTerminated familyName
        liftIO [C.exp| SkTypeface* {
          $(SkFontMgr* fmgr)->matchFamilyStyle(
            $(const char* familyName'),
            *$(SkFontStyle* fontStyle')
          ).release()
        }|]
    )
    SkRefCnt.decrement
