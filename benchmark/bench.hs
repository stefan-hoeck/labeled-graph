{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Graph.Cycles
import Data.Graph.Simple.Graph
import Data.Monoid ((<>))
import Data.Time.Clock
import Data.Tree (drawForest, Forest)


main ∷ IO ()
main = let draw f = putStrLn $ drawForest $ (fmap . fmap) show f
       in  mapM_ draw $ reducedCycleGraph testG

benchConList ∷ Int → IO ()
benchConList o = doBench "ConList" o f
  where f o' = length $ neighbors (complete o') $ vertex 0

doBench ∷ Show a ⇒ String → a → (a → b) → IO ()
doBench msg ini f = do
  start ← getCurrentTime
  let !_ = f ini
  end   ← getCurrentTime
  let diff = diffUTCTime end start
  putStrLn $ msg <> " with param " <> show ini <> " took " <> show diff

testG ∷ Graph
testG = fromList' [0 <-> 1, 1 <-> 2, 2 <-> 3, 1 <-> 4, 4 <-> 5,
                   5 <-> 6, 6 <-> 7, 7 <-> 8, 8 <-> 9, 9 <-> 10,
                   0 <-> 10, 3 <-> 11, 11 <-> 12, 3 <-> 12, 1 <-> 7]
