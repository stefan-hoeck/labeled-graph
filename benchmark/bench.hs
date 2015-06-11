{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq (NFData, force)
import Data.Graph.Simple.Graph
import Data.Graph.Graphs
import Data.Monoid ((<>))
import Data.Time.Clock


main ∷ IO ()
main = doBench "Cycles" "strychnine" pinene (many 1000 cyclesBrute) >>
       doBench "Cycle base" "strychnine" pinene (many 1000 bcycleBase)

-- benchConList ∷ Int → IO ()
-- benchConList o = doBench "ConList" o f
--   where f o' = length $ neighbors (complete o') $ vertex 0

benchShortestPaths ∷ Graph → Int
benchShortestPaths = length . flip shortestPaths 0

many ∷ Int → (a → Int) → a → Int
many 0 _ _ = 0
many n f a = f a + many (n-1) f a

cyclesBrute ∷ Graph → Int
cyclesBrute g = length $ concatMap (cycles g) $ vertices g

bcycleBase ∷ Graph → Int
bcycleBase = length . cycleBase

doBench ∷ (Show a, Show c, NFData b, NFData c) ⇒ String → a → b → (b → c) → IO ()
doBench msg param b f = do
  let !b' = force b
  start ← getCurrentTime
  let !c = force $ f b'
  end   ← getCurrentTime
  let diff = diffUTCTime end start
  putStrLn $ msg <> " with param " 
                 <> show param
                 <> " took " 
                 <> show diff
                 <> ": "
                 <> show c
