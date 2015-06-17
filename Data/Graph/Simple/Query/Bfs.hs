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


-- | Breadth first search implementation
bfs ∷ Graph → [Vertex] → [[Vertex]]
bfs = runBfs


bfPaths ∷ Graph → [Vertex] → [[[Vertex]]]
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


runBfPaths ∷ Graph → [Vertex] → [[[Vertex]]]
runBfPaths g vs = runM (order g) False $ run vs
  where run []    = return []
        run (v:t) = ifM (visited v) (run t) ((:) <$> runS v <*> run t)
        runS v    = visit v >> runQ (singleton [v])
        runQ q = case dequeue q of
                   Just (q',ps@(v:_)) → do ns   ← filterM unvisited $ neighbors g v
                                           mapM_ visit ns
                                           let ps' = fmap (:ps) ns
                                           fmap (ps' ++) $ runQ (enqueueAll q' ps')
                   _                  → return []
