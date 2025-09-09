module Skia.Internal.Prelude (
  module Skia.Core,
  module Skia.Internal.CoreUtils,
  module Skia.Internal.Utils,
  module Skia.Objects,
  module Skia.Errors,

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

import Skia.Core
import Skia.Internal.CoreUtils
import Skia.Internal.Utils
import Skia.Objects
import Skia.Errors

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
