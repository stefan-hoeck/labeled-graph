{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq (NFData, force)
import Data.Graph.Cycles
import Data.Graph.Simple.Graph
import Data.Monoid ((<>))
import Data.Time.Clock
import Data.Tree


main ∷ IO ()
main = let test n = doBench "Shortest paths" n (chain n) benchShortestPaths
       in  mapM_ test [10000,20000..1000000]

-- benchConList ∷ Int → IO ()
-- benchConList o = doBench "ConList" o f
--   where f o' = length $ neighbors (complete o') $ vertex 0

benchShortestPaths ∷ Graph → Int
benchShortestPaths = length . flip shortestPaths 0

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

testG ∷ Graph
testG = fromList' [0 <-> 1, 1 <-> 2, 2 <-> 3, 1 <-> 4, 4 <-> 5,
                   5 <-> 6, 6 <-> 7, 7 <-> 8, 8 <-> 9, 9 <-> 10,
                   0 <-> 10, 3 <-> 11, 11 <-> 12, 3 <-> 12, 1 <-> 7]

strychnine ∷ Graph
strychnine = fromList' [ 0 <-> 1
                       , 0 <-> 5
                       , 1 <-> 2
                       , 2 <-> 3
                       , 3 <-> 4
                       , 4 <-> 5
                       , 4 <-> 8
                       , 5 <-> 6
                       , 6 <-> 7
                       , 6 <-> 15
                       , 6 <-> 16
                       , 7 <-> 8
                       , 7 <-> 12
                       , 8 <-> 9
                       , 9 <-> 10
                       , 9 <-> 24
                       , 10 <-> 11
                       , 11 <-> 12
                       , 11 <-> 23
                       , 12 <-> 13
                       , 13 <-> 14
                       , 13 <-> 20
                       , 14 <-> 15
                       , 15 <-> 18
                       , 16 <-> 17
                       , 17 <-> 18
                       , 18 <-> 19
                       , 19 <-> 20
                       , 20 <-> 21
                       , 21 <-> 22
                       , 22 <-> 23]
