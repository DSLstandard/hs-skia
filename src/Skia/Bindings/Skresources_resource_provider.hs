{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This is an auto-generated FFI binding module.
module Skia.Bindings.Skresources_resource_provider where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void skresources_resource_provider_ref(skresources_resource_provider_t *instance)
@
-}
foreign import ccall "skresources_resource_provider_ref" skresources_resource_provider_ref ::
  Ptr (Skresources_resource_provider) -- ^ C argument @"skresources_resource_provider_t * instance"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skresources_resource_provider_ref'
foreign import ccall "&skresources_resource_provider_ref" p'skresources_resource_provider_ref ::
  FunPtr (Ptr (Skresources_resource_provider) -> IO (()))

{- | C function signature:

@
void skresources_resource_provider_unref(skresources_resource_provider_t *instance)
@
-}
foreign import ccall "skresources_resource_provider_unref" skresources_resource_provider_unref ::
  Ptr (Skresources_resource_provider) -- ^ C argument @"skresources_resource_provider_t * instance"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skresources_resource_provider_unref'
foreign import ccall "&skresources_resource_provider_unref" p'skresources_resource_provider_unref ::
  FunPtr (Ptr (Skresources_resource_provider) -> IO (()))

{- | C function signature:

@
void skresources_resource_provider_delete(skresources_resource_provider_t *instance)
@
-}
foreign import ccall "skresources_resource_provider_delete" skresources_resource_provider_delete ::
  Ptr (Skresources_resource_provider) -- ^ C argument @"skresources_resource_provider_t * instance"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skresources_resource_provider_delete'
foreign import ccall "&skresources_resource_provider_delete" p'skresources_resource_provider_delete ::
  FunPtr (Ptr (Skresources_resource_provider) -> IO (()))

{- | C function signature:

@
sk_data_t *skresources_resource_provider_load(skresources_resource_provider_t *instance, const char *path, const char *name)
@
-}
foreign import ccall "skresources_resource_provider_load" skresources_resource_provider_load ::
  Ptr (Skresources_resource_provider) -- ^ C argument @"skresources_resource_provider_t * instance"@
  -> Ptr (CChar) -- ^ C argument @"const char * path"@
  -> Ptr (CChar) -- ^ C argument @"const char * name"@
  -> IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'skresources_resource_provider_load'
foreign import ccall "&skresources_resource_provider_load" p'skresources_resource_provider_load ::
  FunPtr (Ptr (Skresources_resource_provider) -> Ptr (CChar) -> Ptr (CChar) -> IO (Ptr (Sk_data)))

{- | C function signature:

@
skresources_image_asset_t *skresources_resource_provider_load_image_asset(skresources_resource_provider_t *instance, const char *path, const char *name, const char *id)
@
-}
foreign import ccall "skresources_resource_provider_load_image_asset" skresources_resource_provider_load_image_asset ::
  Ptr (Skresources_resource_provider) -- ^ C argument @"skresources_resource_provider_t * instance"@
  -> Ptr (CChar) -- ^ C argument @"const char * path"@
  -> Ptr (CChar) -- ^ C argument @"const char * name"@
  -> Ptr (CChar) -- ^ C argument @"const char * id"@
  -> IO (Ptr (Skresources_image_asset)) -- ^ C return type: @"skresources_image_asset_t *"@

-- | Function pointer to 'skresources_resource_provider_load_image_asset'
foreign import ccall "&skresources_resource_provider_load_image_asset" p'skresources_resource_provider_load_image_asset ::
  FunPtr (Ptr (Skresources_resource_provider) -> Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> IO (Ptr (Skresources_image_asset)))

{- | C function signature:

@
skresources_external_track_asset_t *skresources_resource_provider_load_audio_asset(skresources_resource_provider_t *instance, const char *path, const char *name, const char *id)
@
-}
foreign import ccall "skresources_resource_provider_load_audio_asset" skresources_resource_provider_load_audio_asset ::
  Ptr (Skresources_resource_provider) -- ^ C argument @"skresources_resource_provider_t * instance"@
  -> Ptr (CChar) -- ^ C argument @"const char * path"@
  -> Ptr (CChar) -- ^ C argument @"const char * name"@
  -> Ptr (CChar) -- ^ C argument @"const char * id"@
  -> IO (Ptr (Skresources_external_track_asset)) -- ^ C return type: @"skresources_external_track_asset_t *"@

-- | Function pointer to 'skresources_resource_provider_load_audio_asset'
foreign import ccall "&skresources_resource_provider_load_audio_asset" p'skresources_resource_provider_load_audio_asset ::
  FunPtr (Ptr (Skresources_resource_provider) -> Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> IO (Ptr (Skresources_external_track_asset)))

{- | C function signature:

@
sk_typeface_t *skresources_resource_provider_load_typeface(skresources_resource_provider_t *instance, const char *name, const char *url)
@
-}
foreign import ccall "skresources_resource_provider_load_typeface" skresources_resource_provider_load_typeface ::
  Ptr (Skresources_resource_provider) -- ^ C argument @"skresources_resource_provider_t * instance"@
  -> Ptr (CChar) -- ^ C argument @"const char * name"@
  -> Ptr (CChar) -- ^ C argument @"const char * url"@
  -> IO (Ptr (Sk_typeface)) -- ^ C return type: @"sk_typeface_t *"@

-- | Function pointer to 'skresources_resource_provider_load_typeface'
foreign import ccall "&skresources_resource_provider_load_typeface" p'skresources_resource_provider_load_typeface ::
  FunPtr (Ptr (Skresources_resource_provider) -> Ptr (CChar) -> Ptr (CChar) -> IO (Ptr (Sk_typeface)))

{- | C function signature:

@
skresources_resource_provider_t *skresources_file_resource_provider_make(sk_string_t *base_dir, _Bool predecode)
@
-}
foreign import ccall "skresources_file_resource_provider_make" skresources_file_resource_provider_make ::
  Ptr (Sk_string) -- ^ C argument @"sk_string_t * base_dir"@
  -> CBool -- ^ C argument @"_Bool predecode"@
  -> IO (Ptr (Skresources_resource_provider)) -- ^ C return type: @"skresources_resource_provider_t *"@

-- | Function pointer to 'skresources_file_resource_provider_make'
foreign import ccall "&skresources_file_resource_provider_make" p'skresources_file_resource_provider_make ::
  FunPtr (Ptr (Sk_string) -> CBool -> IO (Ptr (Skresources_resource_provider)))

{- | C function signature:

@
skresources_resource_provider_t *skresources_caching_resource_provider_proxy_make(skresources_resource_provider_t *rp)
@
-}
foreign import ccall "skresources_caching_resource_provider_proxy_make" skresources_caching_resource_provider_proxy_make ::
  Ptr (Skresources_resource_provider) -- ^ C argument @"skresources_resource_provider_t * rp"@
  -> IO (Ptr (Skresources_resource_provider)) -- ^ C return type: @"skresources_resource_provider_t *"@

-- | Function pointer to 'skresources_caching_resource_provider_proxy_make'
foreign import ccall "&skresources_caching_resource_provider_proxy_make" p'skresources_caching_resource_provider_proxy_make ::
  FunPtr (Ptr (Skresources_resource_provider) -> IO (Ptr (Skresources_resource_provider)))

{- | C function signature:

@
skresources_resource_provider_t *skresources_data_uri_resource_provider_proxy_make(skresources_resource_provider_t *rp, _Bool predecode)
@
-}
foreign import ccall "skresources_data_uri_resource_provider_proxy_make" skresources_data_uri_resource_provider_proxy_make ::
  Ptr (Skresources_resource_provider) -- ^ C argument @"skresources_resource_provider_t * rp"@
  -> CBool -- ^ C argument @"_Bool predecode"@
  -> IO (Ptr (Skresources_resource_provider)) -- ^ C return type: @"skresources_resource_provider_t *"@

-- | Function pointer to 'skresources_data_uri_resource_provider_proxy_make'
foreign import ccall "&skresources_data_uri_resource_provider_proxy_make" p'skresources_data_uri_resource_provider_proxy_make ::
  FunPtr (Ptr (Skresources_resource_provider) -> CBool -> IO (Ptr (Skresources_resource_provider)))
