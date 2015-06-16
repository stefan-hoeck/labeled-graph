{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module        : Data.Graph.Simple.Util
Description   : Utility functions used in many algorithms in this library
Copyright     : Stefan Höck
Maintainer    : Stefan Höck
Stability     : experimental

Functions for internals use. Many are not total so use with care.
-}
module Data.Graph.Simple.Util (

-- * Monad utility functions
  ifM, whenM, unlessM

-- * Operations on sorted lists without dublicates
, unique, sortedUnique, sortedDiff, sortedUnion, sortedSymmDiff

-- * Pretty printing
, rightPad

-- * Mutable vector utility functions
, unsafeReadV, unsafeReadVU
, unsafeWriteV, unsafeWriteVU
, unsafeModU, unsafeMod, unsafeModV, unsafeModVU


, boolMap, partMap

, SetM(..), runM, runMV, setM, getM, modM, visit, visited, unvisited, unvisit
) where

import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Data.Graph.Simple.Vertex (Vertex, unVertex)
import Data.List (sort)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU


-- * Monad utility functions

-- | Depending on the 'Bool' returned by the first monadic
--   action, either performs the second (in case of 'True')
--   or the third provided action.
ifM ∷ Monad m ⇒ m Bool → m a → m a → m a
ifM mb m1 m2 = mb >>= (\b → if b then m1 else m2)

-- | Like 'when' but with a monadic 'Bool' as its first argument
whenM ∷ Monad m ⇒ m Bool → m () → m ()
whenM mb mu = mb >>= (\b → when b mu)

-- | Like 'unless' but with a monadic 'Bool' as its first argument
unlessM ∷ Monad m ⇒ m Bool → m () → m ()
unlessM mb mu = mb >>= (\b → unless b mu)




-- * Operations on sorted lists without dublicates

-- | Sorts a list and removes duplicates
sortedUnique ∷ Ord a ⇒ [a] → [a]
sortedUnique = unique . sort


-- | Removes duplicates from a sorted list
unique ∷ Eq a ⇒ [a] → [a]
unique = run [] where
  run r (x:y:t) | x == y = run r     (y:t)
  run r (h:t)            = run (h:r) t
  run r []               = reverse r


-- | Subtracts the content of the second from the first
--   list in O(m+n). Lists are assumed to be set-like: Sorted and
--   holding each element only once
sortedDiff ∷ Ord a ⇒ [a] → [a] → [a]
sortedDiff = run []
  where run r [] _                            = reverse r
        run r as []                           = reverse r ++ as
        run r as@(a:ta) bs@(b:tb) | a == b    = run r     ta tb
                                  | a <  b    = run (a:r) ta bs
                                  | otherwise = run r     as tb


-- | Combines the content of two lists in O(m+n).
--   Lists are assumed to be set-like: Sorted and
--   holding each element only once
sortedUnion ∷ Ord a ⇒ [a] → [a] → [a]
sortedUnion = run []
  where run r [] bs                           = reverse r ++ bs
        run r as []                           = reverse r ++ as
        run r as@(a:ta) bs@(b:tb) | a <=  b   = run (a:r) ta bs
                                  | otherwise = run r     as tb

-- | Creates the symmetric set difference of to lists.
--   Lists are assumed to be set-like: Sorted and
--   holding each element only once
sortedSymmDiff ∷ Ord a ⇒ [a] → [a] → [a]
sortedSymmDiff = run []
  where run r [] e2                           = reverse r ++ e2
        run r e1 []                           = run r     [] e1
        run r as@(a:ta) bs@(b:tb) | a == b    = run r     ta tb
                                  | a <  b    = run (a:r) ta bs
                                  | otherwise = run (b:r) as tb



-- * Pretty printing

rightPad ∷ a → [[a]] → [[a]]
rightPad _ [] = []
rightPad a as = fmap pad as
    where pad as' = as' ++ replicate (ml - length as') a
          ml      = maximum $ fmap length as



-- * Mutable vector utility functions

-- | Modfies a value in a mutable unboxed array without checking
--   the index first
{-# INLINE unsafeModU #-}
unsafeModU ∷ MVU.Unbox a ⇒ MVU.MVector s a → Int → (a → a) → ST s ()
unsafeModU v i f = MVU.unsafeRead v i >>= MVU.unsafeWrite v i . f


-- | Modfies a value in a mutable unboxed array without checking
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


-- | Extracts the value from a mutable array without
--   checking the index first. Uses a Vertex for indexing.
{-# INLINE unsafeReadV #-}
unsafeReadV ∷ MV.MVector s a → Vertex → ST s a
unsafeReadV v = MV.unsafeRead v . unVertex


-- | Extracts the value from a mutable unboxed array without
--   checking the index first. Uses a Vertex for indexing.
{-# INLINE unsafeReadVU #-}
unsafeReadVU ∷ MVU.Unbox a ⇒ MVU.MVector s a → Vertex → ST s a
unsafeReadVU v = MVU.unsafeRead v . unVertex


-- | Writes a value to a mutable array without
--   checking the index first. Uses a Vertex for indexing.
{-# INLINE unsafeWriteV #-}
unsafeWriteV ∷ MV.MVector s a → Vertex → a → ST s ()
unsafeWriteV v i a = MV.unsafeWrite v (unVertex i) a


-- | Writes a value to a mutable unboxed array without
--   checking the index first. Uses a Vertex for indexing.
{-# INLINE unsafeWriteVU #-}
unsafeWriteVU ∷ MVU.Unbox a ⇒ MVU.MVector s a → Vertex → a → ST s ()
unsafeWriteVU v i a = MVU.unsafeWrite v (unVertex i) a


-- | Returns an efficient mapping from index to 'Bool'.
--   Indices given as a list of vertices will be mapped to 'True'
--   all others to 'False'
boolMap ∷ Int → [Vertex] → VU.Vector Bool
boolMap n vs = runMV n False $ mapM_ visit vs

-- | Returns an efficient mapping from index to value.
--   A default value is given together with a list of
--   index value pairs.
partMap ∷ Int → a → [(Int,a)] → V.Vector a
partMap n ini ps = runST $ do v ← MV.replicate n ini
                              mapM_ (\(i,a) → MV.write v i a) ps
                              V.unsafeFreeze v

-- Used to mark or count visited vertices in graph algorithms
newtype SetM s u a = SetM { runSetM ∷ MVU.MVector s u → ST s a }

instance MVU.Unbox u ⇒ Functor (SetM s u) where
  f `fmap` SetM v = SetM $ fmap f . v

instance MVU.Unbox u ⇒ Applicative (SetM s u) where
  pure              = SetM . const . return
  SetM f <*> SetM a = SetM $ \v → f v <*> a v

instance MVU.Unbox u ⇒ Monad (SetM s u) where
  return       = pure
  SetM v >>= f = SetM $ \s → do x ← v s
                                runSetM (f x) s

{-# INLINE runM #-}
runM ∷ MVU.Unbox u ⇒ Int → u → (forall s . SetM s u a) → a
runM n ini act = runST $ do v ← MVU.replicate n ini
                            runSetM act v

{-# INLINE runMV #-}
runMV ∷ MVU.Unbox u ⇒ Int → u → (forall s . SetM s u ()) → VU.Vector u
runMV n ini act = runST $ do v ← MVU.replicate n ini
                             runSetM act v
                             VU.unsafeFreeze v

{-# INLINE getM #-}
getM ∷ MVU.Unbox u ⇒ Vertex → SetM s u u
getM v = SetM $ \us → unsafeReadVU us v

{-# INLINE setM #-}
setM ∷ MVU.Unbox u ⇒ u → Vertex → SetM s u ()
setM v u = SetM $ \us → unsafeWriteVU us u v

{-# INLINE modM #-}
modM ∷ MVU.Unbox u ⇒ (u → u) → Vertex → SetM s u ()
modM f v = SetM $ \us → unsafeModVU us v f

{-# INLINE visited #-}
visited ∷ Vertex → SetM s Bool Bool
visited = getM

{-# INLINE unvisited #-}
unvisited ∷ Vertex → SetM s Bool Bool
unvisited = fmap not . visited

{-# INLINE visit #-}
visit ∷ Vertex → SetM s Bool ()
visit = setM True

{-# INLINE unvisit #-}
unvisit ∷ Vertex → SetM s Bool ()
unvisit = setM False
