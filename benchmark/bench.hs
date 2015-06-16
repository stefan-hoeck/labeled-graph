{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Data.Graph.Simple
import Data.Graph.Graphs (strychnine)


main ∷ IO ()
main = defaultMain [dfs_bench, cycle_bench]

----------------------------------------------------------------------
-- Depth first search

dfs_bench ∷ Benchmark
dfs_bench = bgroup "dfs" [
              bgroup "complete"       $ fmap b_dfs_c    [10,100,1000]
            , bgroup "complete"       $ fmap b_dff_c    [10,100,1000]
            , bgroup "empty"          $ fmap b_dfs_e    [10,100,1000]
            , bgroup "empty"          $ fmap b_dff_e    [10,100,1000] ]

b_dfs_c ∷ Int → Benchmark
b_dfs_c n = let c = complete n
            in  bench (show n) $ nf (dfs c) (vertices c)

b_dff_c ∷ Int → Benchmark
b_dff_c n = let c = complete n
            in  bench (show n) $ nf (dff c) (vertices c)

b_dfs_e ∷ Int → Benchmark
b_dfs_e n = let e = empty n
            in bench (show n) $ nf (dfs e) (vertices e)

b_dff_e ∷ Int → Benchmark
b_dff_e n = let e = empty n
            in bench (show n) $ nf (dff e) (vertices e)

----------------------------------------------------------------------
-- Cycles

cycle_bench ∷ Benchmark
cycle_bench = bgroup "cycles" [
                bgroup "complete" $ fmap b_cycleBase_c [10,100,1000]
              , bgroup "strych"   $ fmap b_cycleBase_s [10,100,1000]]

b_cycleBase_c ∷ Int → Benchmark
b_cycleBase_c n = let c = complete n
                  in  bench (show n) $ nf cycleBases c

b_cycleBase_s ∷ Int → Benchmark
b_cycleBase_s n = let gs = replicate n strychnine
                      f  = fmap cycleBases
                  in  bench (show n) $ nf f gs
