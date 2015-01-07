module Data.Graph.Simple.Util (
  unique, sortedUnique

, unsafeMod, unsafeModV
) where

import Control.Monad ((>>=))
import Control.Monad.ST (ST)
import Data.Eq (Eq, (==))
import Data.Function ((.))
import Data.Graph.Simple.Vertex (Vertex, unVertex)
import Data.Int (Int)
import Data.List (reverse, sort)
import Data.Ord (Ord)
import qualified Data.Vector.Unboxed.Mutable as MV

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
{-# INLINE unsafeMod #-}
unsafeMod ∷ MV.Unbox a ⇒ MV.MVector s a → Int → (a → a) → ST s ()
unsafeMod v i f = MV.unsafeRead v i >>= MV.unsafeWrite v i . f

-- | Modfies a value in a mutable array without checking
--   the index first
{-# INLINE unsafeModV #-}
unsafeModV ∷ MV.Unbox a ⇒ MV.MVector s a → Vertex → (a → a) → ST s ()
unsafeModV v = unsafeMod v . unVertex
