{- | Importing this module automatically imports other commonly used modules.
This is a helper module for quickly importing a bunch utilities for developing
Skia function bindings with. This module should only be used by this Haskell
library.
-}
module Skia.Internal.Prelude (
  -- * Skia core module reexports.
  module Skia.Core,
  module Skia.Enums,
  module Skia.Errors,
  module Skia.Internal.CoreUtils,
  module Skia.Internal.Utils,
  module Skia.Linear,
  module Skia.Objects,

  -- * Common utils reexports.
  module Control.Exception,
  module Control.Monad,
  module Control.Monad.IO.Class,
  module Control.Monad.Managed,
  module Control.Monad.Trans.Resource,
  module Data.Acquire,
  module Data.Bool,
  module Data.Coerce,
  module Data.Functor,
  module Data.Word,
  module Foreign.C.String,
  module Foreign.C.Types,
  module Foreign.Marshal,
  module Foreign.Ptr,
  module Foreign.Storable,
  module Linear,
) where

-- NOTE: Beware of accidential cyclic imports.

import Skia.Core
import Skia.Enums
import Skia.Errors
import Skia.Internal.CoreUtils
import Skia.Internal.Utils
import Skia.Linear
import Skia.Objects

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed hiding (with)
import Control.Monad.Trans.Resource
import Data.Acquire hiding (with)
import Data.Bool
import Data.Coerce
import Data.Functor
import Data.Word
import Foreign.Marshal hiding (void)
import Linear

-- import Foreign.*
--
-- NOTE: Do not reexport 'import Foreign' directly. This is because it
-- contaminates namespaces with a lot of garbage.

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
