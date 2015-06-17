{- |
Module        : Data.Graph.Simple.Query.Bfs
Description   : Breadth first search and related algorithms
Copyright     : Stefan Höck
Maintainer    : Stefan Höck
Stability     : experimental
-}
module Data.Graph.Simple.Query.Bfs (
  bfs
, bfPaths
) where

import Control.Monad (filterM)
import Data.Queue
import Data.Graph.Simple.Graph hiding (null)
import Data.Graph.Simple.Util


bfs ∷ Graph → [Vertex] → [[Vertex]]
bfs = runBfs


bfPaths ∷ Graph → Vertex → [[Vertex]]
bfPaths = runBfPaths



----------------------------------------------------------------------
-- Algorithms

runBfs ∷ Graph → [Vertex] → [[Vertex]]
runBfs g vs = filter (not . null) $ runM (order g) False $ runAll vs where
  runAll      = mapM runSingle
  runSingle v = ifM (visited v) (return []) (visit v >> run (singleton v))
  run q       = case dequeue q of
                  Nothing      → return []
                  Just (q', v) → do ns ← filterM unvisited $ neighbors g v
                                    mapM_ visit ns
                                    fmap (v:) (run $ enqueueAll q' ns)

runBfPaths ∷ Graph → Vertex → [[Vertex]]
runBfPaths g v = runM (order g) False (visit v >> run (singleton [v])) where
  run q = case dequeue q of
            Nothing       → return []
            Just (q', vs) → do ns ← filterM unvisited $ neighbors g (head vs)
                               mapM_ visit ns
                               let ns' = fmap (:vs) ns
                               fmap (vs:) (run $ enqueueAll q' ns')
