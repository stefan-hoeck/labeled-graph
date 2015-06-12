{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Data.Graph.Simple


main ∷ IO ()
main = defaultMain [
         bgroup "dfs_complete"       $ fmap b_dfs_c    [10,100,1000]
       , bgroup "dff_complete"       $ fmap b_dff_c    [10,100,1000]
       , bgroup "dfs_empty"          $ fmap b_dfs_e    [10,100,1000]
       , bgroup "dff_empty"          $ fmap b_dff_e    [10,100,1000]
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
