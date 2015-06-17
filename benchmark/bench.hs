{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Data.Graph.Simple
import Data.Graph.Graphs (strychnine)


main ∷ IO ()
main = defaultMain [
         dfs_group
       , bfs_group
       , cycle_group]


----------------------------------------------------------------------
-- Depth first search

dfs_group ∷ Benchmark
dfs_group = bgroup "dfs" 
  [ bgroup "dfs_complete" $ fmap b_dfs_c    [10,100,1000]
  , bgroup "dff_complete" $ fmap b_dff_c    [10,100,1000]
  , bgroup "dfs_empty"    $ fmap b_dfs_e    [10,100,1000]
  , bgroup "dff_empty"    $ fmap b_dff_e    [10,100,1000]
  ]

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
-- Breadth first search

bfs_group ∷ Benchmark
bfs_group = bgroup "bfs" 
  [ bgroup "bfs_complete"       $ fmap b_bfs_c    [10,100,1000]
  , bgroup "bfPaths_complete"   $ fmap b_bfp_c    [10,100,1000]
  , bgroup "bfs_empty"          $ fmap b_bfs_e    [10,100,1000]
  , bgroup "bfPaths_empty"      $ fmap b_bfp_e    [10,100,1000]
  ]

b_bfs_c ∷ Int → Benchmark
b_bfs_c n = let c = complete n
            in  bench (show n) $ nf (bfs c) (vertices c)

b_bfp_c ∷ Int → Benchmark
b_bfp_c n = let c = complete n
            in  bench (show n) $ nf (bfPaths c) (vertices c)

b_bfs_e ∷ Int → Benchmark
b_bfs_e n = let e = empty n
            in bench (show n) $ nf (bfs e) (vertices e)

b_bfp_e ∷ Int → Benchmark
b_bfp_e n = let e = empty n
            in bench (show n) $ nf (bfPaths e) (vertices e)


----------------------------------------------------------------------
-- Cycles

cycle_group ∷ Benchmark
cycle_group = bgroup "cycles" [
                bgroup "complete" $ fmap b_cycleBase_c [10,100,1000]
              , bgroup "strych"   $ fmap b_cycleBase_s [10,100,1000]]

b_cycleBase_c ∷ Int → Benchmark
b_cycleBase_c n = let c = complete n
                  in  bench (show n) $ nf cycleBases c

b_cycleBase_s ∷ Int → Benchmark
b_cycleBase_s n = let gs = replicate n strychnine
                      f  = fmap cycleBases
                  in  bench (show n) $ nf f gs
