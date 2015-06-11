{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq (NFData, force)
import Data.Graph.Graphs
import Data.Graph.Simple.Graph
import Data.Monoid ((<>))
import Data.Time.Clock


main ∷ IO ()
main = let test n = doBench "cyclic subgraphs" n (alkane n) benchCyclicSubgraphs
       in  mapM_ test [1000,2000..10000]

benchShortestPaths ∷ Graph → Int
benchShortestPaths = length . flip shortestPaths 0

benchCyclicSubgraphs ∷ Graph → Int
benchCyclicSubgraphs = sum . fmap (size . fst) . cyclicSubgraphs

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
