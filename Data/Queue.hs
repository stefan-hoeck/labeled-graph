{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Queue (
  Queue

, empty, singleton, fromList

, toList, enqueue, dequeue, enqueueAll
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Queue a = Q [a] [a]
  deriving (Show, Eq, Functor, NFData, Generic)

empty ∷ Queue a
empty = Q [] []

singleton ∷ a → Queue a
singleton a = Q [a] []

fromList ∷ [a] → Queue a
fromList as = Q as []

toList ∷ Queue a → [a]
toList (Q h t) = h ++ reverse t

enqueue ∷ Queue a → a → Queue a
enqueue (Q h t) a = Q (a:h) t

enqueueAll ∷ Queue a → [a] → Queue a
enqueueAll (Q h t) as = Q (as ++ h) t

dequeue ∷ Queue a → Maybe (Queue a, a)
dequeue (Q [] [])     = Nothing
dequeue (Q h  [])     = let (h':t) = reverse h
                        in  Just (Q [] t, h')
dequeue (Q h  (h':t)) = Just (Q h t, h')
