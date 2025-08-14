module Skia.SKFontManager where

import Control.Monad.Trans.Resource
import Data.BCP47 qualified as BCP47
import Data.Char qualified
import Data.Text qualified as T
import Data.Traversable
import Skia.Bindings.Sk_fontmgr
import Skia.Bindings.Sk_typeface
import Skia.Internal.Prelude
import Skia.SKFontStyle
import Skia.SKRefCnt qualified as SKRefCnt
import Skia.SKString qualified as SKString

-- * Creating SKFontManager

createByFontconfig :: (MonadResource m) => m (ReleaseKey, SKFontManager)
createByFontconfig = do
  allocateSKObjectOrErrorIfNull'
    "Failed to create SKFontManager with Fontconfig as backend"
    skfontmgr_new_fontconfig
    SKRefCnt.decrement

createByGDI :: (MonadResource m) => m (ReleaseKey, SKFontManager)
createByGDI = do
  allocateSKObjectOrErrorIfNull'
    "Failed to create SKFontManager with Windows GDI as backend"
    skfontmgr_new_gdi
    SKRefCnt.decrement

-- * Checking if a particular backend is supported

isFontconfigSupported :: (MonadIO m) => m Bool
isFontconfigSupported = liftIO $ toBool <$> skfontmgr_is_supported_fontconfig

isGDISupported :: (MonadIO m) => m Bool
isGDISupported = liftIO $ toBool <$> skfontmgr_is_supported_gdi

-- * SKFontManager functions

countFamilies :: (MonadIO m) => SKFontManager -> m Int
countFamilies fmgr = liftIO do
  fromIntegral <$> sk_fontmgr_count_families (ptr fmgr)

getFamilyName ::
  (MonadIO m) =>
  SKFontManager ->
  -- | Index
  Int ->
  m T.Text
getFamilyName fmgr index = liftIO $ runResourceT do
  -- TODO: Google Skia does not document this function. What happens if
  -- 'index' is invalid?
  (_, familyNameStr) <- SKString.createEmpty
  liftIO $ sk_fontmgr_get_family_name (ptr fmgr) (fromIntegral index) (ptr familyNameStr)
  SKString.getAsText familyNameStr

createStyleSet ::
  (MonadResource m) =>
  SKFontManager ->
  -- | Index
  Int ->
  m (ReleaseKey, SKFontStyleSet)
createStyleSet fmgr index =
  -- TODO: Google Skia does not document this function. What happens if
  -- 'index' is invalid?
  allocateSKObjectNeverNull'
    (sk_fontmgr_create_styleset (ptr fmgr) (fromIntegral index))
    SKRefCnt.decrement

{- | Returns an empty set if the name is not found.

Passing 'Nothing' as the family name will return the default system family. Note
that most systems don't have a default system family, so passing nullptr will
often result in the empty set.

It is possible that this will return a style set not accessible from
'createStyleSet' due to hidden or auto-activated fonts.
-}
matchFamily ::
  (MonadResource m) =>
  SKFontManager ->
  -- | Family name
  Maybe T.Text ->
  m (ReleaseKey, SKFontStyleSet)
matchFamily fmgr familyName =
  -- Google Skia on matchFamily(): Never returns NULL; will return an empty
  -- set if the name is not found.
  allocateSKObjectNeverNull'
    ( evalManaged do
        familyName' <- useNullIfNothing storableTextUTF8NullTerminated familyName
        liftIO $ sk_fontmgr_match_family (ptr fmgr) familyName'
    )
    SKRefCnt.decrement

{- | Finds the closest matching typeface to the specified family anme and style
and returns it. Will return 'Nothing' if no \'good\' match is found.

Passing 'Nothing' as the family name will return the default system font.

It is possible that this will return a style set not accessible from
"createStyleSet' or 'matchFamily' due to hidden or auto-activated fonts.
-}
matchFamilyStyle ::
  (MonadResource m) =>
  SKFontManager ->
  -- | Family name
  Maybe T.Text ->
  SKFontStyle ->
  m (Maybe (ReleaseKey, SKTypeface))
matchFamilyStyle fmgr familyName fontStyle =
  allocateSKObjectOrNothingIfNull'
    ( evalManaged do
        fontStyle' <- marshalSKFontStyle fontStyle
        familyName' <- useNullIfNothing storableTextUTF8NullTerminated familyName
        liftIO $ sk_fontmgr_match_family_style (ptr fmgr) familyName' fontStyle'
    )
    SKRefCnt.decrement

{- | Use the system fallback to find a typeface for the given character.

Will return 'Nothing' if no family can be found for the character in the system
fallback.

Passing 'Nothing' as the family name will return the default system font.

bcp47[0] is the least significant fallback, bcp47[bcp47Count-1] is the most
significant. If no specified bcp47 codes match, any font with the requested
character will be matched.
-}
matchFamilyStyleCharacter ::
  (MonadResource m) =>
  SKFontManager ->
  -- | Family name
  Maybe T.Text ->
  SKFontStyle ->
  [BCP47.BCP47] ->
  Char ->
  m (Maybe (ReleaseKey, SKTypeface))
matchFamilyStyleCharacter fmgr familyName fontStyle inputBcp47s char =
  allocateSKObjectOrNothingIfNull'
    ( evalManaged do
        familyName' <- useNullIfNothing storableTextUTF8NullTerminated familyName
        fontStyle' <- marshalSKFontStyle fontStyle

        bcp47Strings <- for inputBcp47s \bcp47 -> storableTextUTF8NullTerminated (BCP47.toText bcp47)
        (bcp47', bcp47Count) <- managed $ withArrayLen' bcp47Strings

        liftIO $
          sk_fontmgr_match_family_style_character
            (ptr fmgr)
            familyName'
            fontStyle'
            bcp47'
            (fromIntegral bcp47Count)
            (fromIntegral (Data.Char.ord char))
    )
    SKRefCnt.decrement

{- | Returns a typeface for the specified file name and TTC index (pass
'Nothing' for none). Returns 'Nothing' if the file is not found, or its contents
are not recognized.
-}
createFromFile ::
  (MonadResource m) =>
  SKFontManager ->
  FilePath ->
  -- | TTC index
  Maybe Int ->
  m (ReleaseKey, SKTypeface)
createFromFile fmgr path ttcIndex =
  allocateSKObjectOrErrorIfNull'
    "Cannot create SKTypeface from the specified file and TTC index. Either the file is not found or its contents are not recognized."
    ( evalManaged do
        path' <- managed $ withCString path
        liftIO $ sk_fontmgr_create_from_file (ptr fmgr) path' (maybe 0 fromIntegral ttcIndex)
    )
    SKRefCnt.decrement

{- | Returns a typeface for the specified stream and TTC index (pass 'Nothing'
for none). Returns 'Nothing' if the file is not found, or its contents are not
recognized.
-}
createFromStream ::
  (MonadResource m, IsSKStreamAsset stream) =>
  SKFontManager ->
  stream ->
  -- | TTC index
  Maybe Int ->
  m (ReleaseKey, SKTypeface)
createFromStream fmgr (toA SKStreamAsset -> stream) ttcIndex =
  allocateSKObjectOrErrorIfNull'
    "Cannot create SKTypeface from the specified stream and TTC index. Either the file is not found or its contents are not recognized."
    (sk_fontmgr_create_from_stream (ptr fmgr) (ptr stream) (maybe 0 fromIntegral ttcIndex))
    SKRefCnt.decrement

{- | Returns a typeface for the specified data and TTC index (pass 'Nothing' for
none). Returns 'Nothing' if the file is not found, or its contents are not
recognized.
-}
createFromData ::
  (MonadResource m) =>
  SKFontManager ->
  SKData ->
  -- | TTC index
  Maybe Int ->
  m (ReleaseKey, SKTypeface)
createFromData fmgr dat ttcIndex =
  allocateSKObjectOrErrorIfNull'
    "Cannot create SKTypeface from the specified data and TTC index. Either the file is not found or its contents are not recognized."
    (sk_fontmgr_create_from_data (ptr fmgr) (ptr dat) (maybe 0 fromIntegral ttcIndex))
    SKRefCnt.decrement
