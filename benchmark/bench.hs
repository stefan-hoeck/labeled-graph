{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Foldable (forM_)
import Data.Graph.Simple.Graph
import Data.Monoid ((<>))
import Data.Time.Clock


main ∷ IO ()
main = forM_ [500, 1000 .. 2000] benchConList

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
