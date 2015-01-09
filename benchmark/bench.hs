{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Foldable (forM_)
import Data.Function (($), (.), flip)
import Data.Graph.Simple.Graph
import Data.Graph.Simple.Vertex (vertex)
import Data.Int (Int)
import Data.List (length)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Time.Clock
import System.IO (IO, putStrLn)
import Text.Show (Show, show)


main ∷ IO ()
main = forM_ [500, 1000 .. 2000] benchConList

benchConList ∷ Int → IO ()
benchConList o = doBench "ConList" o f
  where f o' = length $ neighbors (complete o') $ vertex 0

doBench ∷ Show a ⇒ String → a → (a → b) → IO ()
doBench msg ini f = do
  start ← getCurrentTime
  let !b = f ini
  end   ← getCurrentTime
  let diff = diffUTCTime end start
  putStrLn $ msg <> " with param " <> show ini <> " took " <> show diff
