module Data.Graph.Simple.Query.Bfs (
  bfs
) where

import Control.Monad (filterM)
import Data.Graph.Simple.Graph
import Data.Graph.Simple.Util
import Data.Queue

-- | Breadth first search implementation
bfs ∷ Graph → [Vertex] → [[[Vertex]]]
bfs = runBfs

----------------------------------------------------------------------
-- Algorithms

runBfs ∷ Graph → [Vertex] → [[[Vertex]]]
runBfs g vs = runM (order g) False $ run vs
  where run []    = return []
        run (v:t) = ifM (visited v) (run t) ((:) <$> runS v <*> run t)
        runS v    = visit v >> runQ (singleton [v])
        runQ q = case dequeue q of
                   Just (q',ps@(v:_)) → do ns   ← filterM unvisited $ neighbors g v
                                           mapM_ visit ns
                                           let ps' = fmap (:ps) ns
                                           fmap (ps' ++) $ runQ (enqueueAll q' ps')
                   _                  → return []
