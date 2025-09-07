-- | Internal utilities used by other parts of this Haskell library.
module Skia.Internal.Utils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed as Managed
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Coerce
import Data.Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Data.Text.Foreign qualified
import Data.Vector.Storable qualified as VS
import Foreign
import Foreign.C.String
import Linear

applyV2 :: V2 a -> (a -> a -> r) -> r
applyV2 (V2 x y) f = f x y
{-# INLINE applyV2 #-}

apply :: a -> (a -> r) -> r
apply x f = f x
{-# INLINE apply #-}

peekWith :: (Storable a, MonadIO m) => (a -> b) -> Ptr a -> m b
peekWith mapping ptr = do
  r <- liftIO $ peek ptr
  pure $ mapping r
{-# INLINE peekWith #-}

{- | Like 'withArrayLen' but the inner function is uncurried.

The purpose of this function is to allow one to write:

@
(ptr, len) <- ContT $ withArrayLen' myHaskellArray
@
-}
withArrayLen' :: (Storable s) => [s] -> ((Ptr s, Int) -> IO r) -> IO r
withArrayLen' array f = withArrayLen array (flip $ curry f)
{-# INLINE withArrayLen' #-}

bitOrs :: (Bits a) => [a] -> a
bitOrs = foldl' (.|.) zeroBits
{-# INLINE bitOrs #-}

makeBitFlags ::
  forall a.
  (Bits a) =>
  -- | A list of (Flag values, True if flag is set)
  [(Bool, a)] ->
  a
makeBitFlags flags = do
  foldl'
    ( \flag (flagIsSet, flagValue) ->
        if flagIsSet
          then flag .|. flagValue
          else flag
    )
    zeroBits
    flags

hasFlag ::
  (Bits a) =>
  -- | Query flag value - the flag value with only one bit set
  a ->
  -- | Flag value input to be tested
  a ->
  Bool
hasFlag query flag = (flag .&. query) /= zeroBits

convert4Word8ToWord32 :: (Word8, Word8, Word8, Word8) -> Word32
convert4Word8ToWord32 (c1, c2, c3, c4) = do
  -- FIXME: Is there already a builtin function that does this?
  (c1' `shiftL` 24) .|. (c2' `shiftL` 16) .|. (c3' `shiftL` 8) .|. c4'
 where
  c1' = fromIntegral c1
  c2' = fromIntegral c2
  c3' = fromIntegral c3
  c4' = fromIntegral c4

convertWord32To4Word8 :: Word32 -> (Word8, Word8, Word8, Word8)
convertWord32To4Word8 word32 = do
  -- FIXME: Is there already a builtin function that does this?
  (octet 3, octet 2, octet 1, octet 0)
 where
  octet :: Int -> Word8
  octet ix = fromIntegral $ word32 `shiftR` (8 * ix)
  {-# INLINE octet #-}

{- | Like 'castPtr' but imposes a 'Coercible' constraint. This function can be
used to catch type errors.
-}
coercePtr :: (Coercible a b) => Ptr a -> Ptr b
coercePtr = castPtr

{- | Like 'castForeignPtr' but imposes a 'Coercible' constraint. This function can be
used to catch type errors.
-}
coerceForeignPtr :: (Coercible a b) => ForeignPtr a -> ForeignPtr b
coerceForeignPtr = castForeignPtr

-- | 'when' but monadic.
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM condition action = do
  condition <- condition
  when condition action

-- | 'unless' but monadic.
unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM condition action = do
  condition <- condition
  unless condition action

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _m = pure ()
whenJust (Just x) m = m x

whenJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM f m = f >>= flip whenJust m

whenNothing :: (Applicative m) => Maybe x -> m () -> m ()
whenNothing Nothing m = m
whenNothing (Just _) _m = pure ()

whenNothingM :: (Monad m) => m (Maybe x) -> m () -> m ()
whenNothingM f m = f >>= flip whenNothing m

whenNotNull :: (Applicative m) => [a] -> (NonEmpty.NonEmpty a -> m ()) -> m ()
whenNotNull [] _m = pure ()
whenNotNull (x : xs) m = m (x NonEmpty.:| xs)

whenNotNullM :: (Monad m) => m [a] -> (NonEmpty.NonEmpty a -> m ()) -> m ()
whenNotNullM f m = f >>= flip whenNotNull m

whenNotNull_ :: (Applicative m) => [a] -> m () -> m ()
whenNotNull_ [] _m = pure ()
whenNotNull_ (_ : _) m = m

whenNotNullM_ :: (Monad m) => m [a] -> m () -> m ()
whenNotNullM_ f m = f >>= flip whenNotNull_ m

whenNull :: (Applicative m) => [x] -> m () -> m ()
whenNull [] m = m
whenNull (_ : _) _m = pure ()

whenNullM :: (Monad m) => m [x] -> m () -> m ()
whenNullM f m = f >>= flip whenNull m

-- * Utils for the @Managed@ monad.

-- | Like 'runManaged' but has 'liftIO' and directly returns the output value (unsafely).
evalManaged :: (MonadIO m) => Managed a -> m a
evalManaged m = liftIO do
  Managed.with m pure

{- | An alias of 'Foreign.with'. This alias can be used in lieu of
@with@, which often has name clashes.
-}
storable :: (Storable a) => a -> Managed (Ptr a)
storable a = managed (Foreign.with a)

storableVector :: (Storable a) => VS.Vector a -> Managed (Ptr a)
storableVector vector = managed $ VS.unsafeWith vector

-- | TODO: This is O(n)
storableTextUTF8Len :: T.Text -> Managed CStringLen
storableTextUTF8Len txt = managed $ Data.Text.Foreign.withCStringLen txt

-- | TODO: This is O(n)
storableTextUTF8NullTerminated :: T.Text -> Managed CString
storableTextUTF8NullTerminated txt = managed $ Data.Text.Foreign.withCString txt

-- | NOTE: This is O(1)
storableByteStringLen :: BS.ByteString -> Managed CStringLen
storableByteStringLen a = managed $ BS.unsafeUseAsCStringLen a

useNullIfNothing :: (input -> Managed (Ptr output)) -> (Maybe input -> Managed (Ptr output))
useNullIfNothing _f Nothing = pure nullPtr
useNullIfNothing f (Just input) = f input

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just
