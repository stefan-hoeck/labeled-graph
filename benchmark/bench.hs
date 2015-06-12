{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Data.Graph.Simple


main ∷ IO ()
main = defaultMain [
         bgroup "dfs_complete" $ fmap b_dfs_c    [100,1000]
       , bgroup "dff_complete" $ fmap b_dff_c    [100,1000]
       , bgroup "dfs_empty"    $ fmap b_dfs_e    [100,1000]
       , bgroup "dff_empty"    $ fmap b_dff_e    [100,1000]
       , bgroup "complete"     $ fmap b_complete [100,1000]
       ]

b_dfs_c ∷ Int → Benchmark
b_dfs_c n = bench (show n) $ nf (dfs $ complete n) [0]

b_dff_c ∷ Int → Benchmark
b_dff_c n = bench (show n) $ nf (dff $ complete n) [0]

b_dfs_e ∷ Int → Benchmark
b_dfs_e n = let e = empty n
            in bench (show n) $ nf (dfs e) (vertices e)

b_dff_e ∷ Int → Benchmark
b_dff_e n = let e = empty n
            in bench (show n) $ nf (dff e) (vertices e)

b_complete ∷ Int → Benchmark
b_complete n = bench (show n) $ nf complete n
