{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Graph.Simple.Util (
  unique, sortedUnique, rightPad
, sortedDiff

, unsafeReadV, unsafeReadVU
, unsafeWriteV, unsafeWriteVU
, unsafeModU, unsafeMod, unsafeModV, unsafeModVU

, ifM, whenM, unlessM

, boolMap
, SetM(..), runM, runMV, setM, getM, modM, visit, visited, unvisit
) where

import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Data.Graph.Simple.Vertex (Vertex, unVertex)
import Data.List (sort)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Unboxed as VU

-- | Sorts a list and removes duplicates
sortedUnique ∷ Ord a ⇒ [a] → [a]
sortedUnique = unique . sort


-- | Removes duplicates from a sorted list
unique ∷ Eq a ⇒ [a] → [a]
unique = run [] where
  run r (x:y:t) | x == y = run r     (y:t)
  run r (h:t)            = run (h:r) t
  run r []               = reverse r

rightPad ∷ a → [[a]] → [[a]]
rightPad _ [] = []
rightPad a as = fmap pad as
    where pad as' = as' ++ replicate (ml - length as') a
          ml      = maximum $ fmap length as

-- | Subtracts the conten of the second from the first
--   list. Lists are assumed to set-like: Sorted and
--   holding each element only once
sortedDiff ∷ Ord a ⇒ [a] → [a] → [a]
sortedDiff = run []
  where run r [] _                            = reverse r
        run r as []                           = reverse r ++ as
        run r as@(a:ta) bs@(b:tb) | a == b    = run r     ta tb
                                  | a <  b    = run (a:r) ta bs
                                  | otherwise = run r     as tb

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

boolMap ∷ Int → [Vertex] → VU.Vector Bool
boolMap n vs = runMV n False $ mapM_ visit vs

ifM ∷ Monad m ⇒ m Bool → m a → m a → m a
ifM mb m1 m2 = mb >>= (\b → if b then m1 else m2)

whenM ∷ Monad m ⇒ m Bool → m () → m ()
whenM mb mu = mb >>= (\b → when b mu)

unlessM ∷ Monad m ⇒ m Bool → m () → m ()
unlessM mb mu = mb >>= (\b → unless b mu)

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

{-# INLINE visit #-}
visit ∷ Vertex → SetM s Bool ()
visit = setM True

{-# INLINE unvisit #-}
unvisit ∷ Vertex → SetM s Bool ()
unvisit = setM False
