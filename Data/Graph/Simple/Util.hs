module Data.Graph.Simple.Util (
  unique, sortedUnique

, unsafeReadV, unsafeReadVU
, unsafeWriteV, unsafeWriteVU
, unsafeModU, unsafeMod, unsafeModV, unsafeModVU
) where

import Control.Monad.ST (ST)
import Data.Graph.Simple.Vertex (Vertex, unVertex)
import Data.List (sort)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MVU

-- | Sorts a list and removes duplicates
sortedUnique ∷ Ord a ⇒ [a] → [a]
sortedUnique = unique . sort


-- | Removes duplicates from a sorted list
unique ∷ Eq a ⇒ [a] → [a]
unique = run [] where
  run r (x:y:t) | x == y = run r     (y:t)
  run r (h:t)            = run (h:r) t
  run r []               = reverse r


-- | Modfies a value in a mutable array without checking
--   the index first
{-# INLINE unsafeModU #-}
unsafeModU ∷ MVU.Unbox a ⇒ MVU.MVector s a → Int → (a → a) → ST s ()
unsafeModU v i f = MVU.unsafeRead v i >>= MVU.unsafeWrite v i . f

-- | Modfies a value in a mutable array without checking
--   the index first. Uses a Vertex for indexing
{-# INLINE unsafeModVU #-}
unsafeModVU ∷ MVU.Unbox a ⇒ MVU.MVector s a → Vertex → (a → a) → ST s ()
unsafeModVU v i f = unsafeReadVU v i >>= unsafeWriteVU v i . f

-- | Modfies a value in a mutable array without checking
--   the index first
{-# INLINE unsafeMod #-}
unsafeMod ∷ MV.MVector s a → Int → (a → a) → ST s ()
unsafeMod v i f = MV.unsafeRead v i >>= MV.unsafeWrite v i . f

-- | Modfies a value in a mutable array without checking
--   the index first. Uses a Vertex for indexing
{-# INLINE unsafeModV #-}
unsafeModV ∷ MV.MVector s a → Vertex → (a → a) → ST s ()
unsafeModV v i f = unsafeReadV v i >>= unsafeWriteV v i . f

{-# INLINE unsafeReadV #-}
unsafeReadV ∷ MV.MVector s a → Vertex → ST s a
unsafeReadV v = MV.unsafeRead v . unVertex

{-# INLINE unsafeReadVU #-}
unsafeReadVU ∷ MVU.Unbox a ⇒ MVU.MVector s a → Vertex → ST s a
unsafeReadVU v = MVU.unsafeRead v . unVertex

{-# INLINE unsafeWriteV #-}
unsafeWriteV ∷ MV.MVector s a → Vertex → a → ST s ()
unsafeWriteV v i a = MV.unsafeWrite v (unVertex i) a

{-# INLINE unsafeWriteVU #-}
unsafeWriteVU ∷ MVU.Unbox a ⇒ MVU.MVector s a → Vertex → a → ST s ()
unsafeWriteVU v i a = MVU.unsafeWrite v (unVertex i) a
