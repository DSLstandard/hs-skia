module Skia.Core where

import Foreign
import GHC.Base
import GHC.TypeError
import Skia.Internal.TypeLevelBool qualified as TB

{- | Newtypes that wrap around a 'Ptr'. Provides universal methods to unwrap and
wrap the pointers.
-}
class PtrNewType s a | s -> a where
  -- | Creates a new instance of the newtype by wrapping the given 'Ptr'.
  fromPtr :: Ptr a -> s

  -- | Returns the underlying 'Ptr'.
  ptr :: s -> Ptr a

-- | Like 'ptr', but returns 'nullPtr' if the input is 'Nothing'.
ptrOrNull :: (PtrNewType s a) => Maybe s -> Ptr a
ptrOrNull Nothing = nullPtr
ptrOrNull (Just object) = ptr object

-- | Enum types in the Skia library.
class SkEnum s a | s -> a where
  marshalSkEnum :: s -> a
  unmarshalSkEnum :: a -> Maybe s

-- | TODO: Document this
class SkObject s where
  fromAnyPtr :: forall a. Ptr a -> s
  toAnyPtr :: forall a. s -> Ptr a

-- -- | DEVNOTE: If HS_SKIA_CHECK_NULLPTR_ENABLED is enabled and the input is an
-- -- nullPtr, we want an IO error instead of simply Haskell's 'error' to guarantee
-- -- that the exception is thrown *in sync*.
-- --
-- -- Therefore, this function cannot be pure.
-- toObject :: (ManagedPtrNewType s a, SkObject s, MonadIO m) => Ptr a -> m s
-- toObject ptr = liftIO do
-- #ifdef HS_SKIA_CHECK_NULLPTR_ENABLED
--     when (ptr == nullPtr) do
--         Control.Exception.throwIO $ InternalError "toObject encountered nullptr"
-- #endif
--     fptr <- newManagedPtr_ ptr
--     pure $ fromManagedPtr fptr

--
-- ### OOP inheritance
--
-- Implementation is adapted from
--  https://hackage.haskell.org/package/haskell-gi-base-0.26.8/docs/Data-GI-Base-Overloading.html.
--
-- Modifications are made to 1) allow transitivity in ancestor checks and 2)
-- improve error messages.
--

{- | All the types that are ascendants of this type, including
interfaces that the type implements.
-}
type family ParentTypes a :: [Type]

{- | Helper type for 'IsDescendantOf'.

Recursively (in the sense that the ParentTypes of parents are also checked)
check if 'parent' is contained in 'parents'.
-}
type family ExistsParent (parent :: Type) (parents :: [Type]) :: Type where
  ExistsParent parent '[] =
    TB.False
  ExistsParent parent (parent ': parents) =
    TB.True
  ExistsParent parent (parent__ ': parents) =
    TB.Or
      (ExistsParent parent (ParentTypes parent__))
      (ExistsParent parent parents)

{- | Check that a type is in the list of `ParentTypes` of another
type.
-}
type family IsDescendantOf (parent :: Type) (t :: Type) :: Type where
  -- Every object is defined to be a descendant of itself.
  IsDescendantOf t t = TB.True
  IsDescendantOf parent t = ExistsParent parent (ParentTypes t)

-- | Used by 'IsSubclassOf' to signal an error.
type TypeError'NotSubclassOf super sub =
  TypeError
    ( 'Text "‘"
        ':<>: 'ShowType sub
        ':<>: 'Text "’ is not a subclass of ‘"
        ':<>: 'ShowType super
        ':<>: 'Text "’"
    )

-- | TODO: Document this
type IsSubclassOf super sub =
  ( SkObject super
  , SkObject sub
  , {-
        Explanation:

        when (IsDescendantOf super sub) is True, this becomes () ~ () and the typechecker is satisfied;

        when (IsDescendantOf super sub) is False, this becomes (TypeError'NotSubclassOf super sub) and the typechecker reports an error.
    -}
    TB.If (IsDescendantOf super sub) () (TypeError'NotSubclassOf super sub) ~ ()
  )

-- | TODO: Document this
asA ::
  ( SkObject super
  , SkObject sub
  , IsSubclassOf super sub
  ) =>
  sub ->
  (Ptr super' -> super) ->
  super
asA sub _constructor = fromAnyPtr $ toAnyPtr sub

-- | TODO: Document this
toA ::
  (IsSubclassOf super sub) =>
  (Ptr super' -> super) ->
  sub ->
  super
toA = flip asA

-- | TODO: Document this
pointerCast ::
  ( IsSubclassOf super sub
  , PtrNewType super super'
  , PtrNewType sub sub'
  ) =>
  (Ptr sub' -> sub) ->
  (Ptr super' -> super) ->
  Ptr sub' ->
  Ptr super'
pointerCast _sub _super = castPtr
