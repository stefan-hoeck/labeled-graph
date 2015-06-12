{- |
Module        : Data.Graph.Simple.Query.Dfs
Description   : Depth first search and related algorithms
Copyright     : Stefan Höck
Maintainer    : Stefan Höck
Stability     : experimental

This module provides algorithms for depth first search,
including an implementation of depth first forests similar to
the one found in 'Data.Graph'.

Some additional related algorithms like testing for connectivity
and extracting connected subgraphs is also provided.
-}
module Data.Graph.Simple.Query.Dfs (
  dfs
, dff
, reachable
, isConnected
, connectedSubgraphs
) where

import Control.Monad (liftM2)
import Data.Graph.Simple.Graph hiding (null)
import Data.Graph.Simple.Util
import Data.Tree (Tree(..), Forest)

-- | Returns for each vertex given all reachable
--   vertices in depth first order. If a vertex
--   appearing later in the list was already visited
--   from an earlier vertex, it is not processed
--   any further.
--
--   Runs in O(|V|+|E|) time
dfs ∷ Graph → [Vertex] → [[Vertex]]
dfs = runDfs


-- | Depth first search forest with roots in the given vertices.
--   If a vertex from the list was already visited from an earlier
--   vertex, its 'Tree' is discarded.
--
--   Runs in O(|V|+|E|) time
dff ∷ Graph → [Vertex] → Forest Vertex
dff g = pruneDff (order g) . fmap (generateTree g)


-- | Returns a list of vertices reachable from a given vertex
--   in depth first order. The list starts with the vertex
--   provided.
reachable ∷ Graph → Vertex → [Vertex]
reachable g v = head $ dfs g [v]


-- | True if the graph is connected
--
--   Runs in O(|V|+|E|) time
isConnected ∷ Graph → Bool
isConnected g = case dfs g $ vertices g of
                     []  → True
                     [_] → True
                     _   → False


-- | Creates the connected components of the given graph.
--
--   Note that the numbering of vertices in the subgraphs will
--   be adjusted.
connectedSubgraphs ∷ Graph → [Graph]
connectedSubgraphs g = case dfs g (vertices g) of
                         []  → []
                         [_] → [g]
                         f   → fmap (inducedSubgraph g) f
                




generateTree ∷ Graph → Vertex → Tree Vertex
generateTree g v  = Node v $ map (generateTree g) (neighbors g v)



----------------------------------------------------------------------
-- Algorithms

runDfs ∷ Graph → [Vertex] → [[Vertex]]
runDfs g vs = filter (not . null) $ runM (order g) False $ runAll vs where
  runAll      = mapM runSingle
  runSingle v = run [v]
  run []      = return []
  run (v:t)   = let t'   = neighbors g v ++ t
                    nvis = visit v >> fmap (v:) (run t')
                in  ifM (visited v) (run t) nvis


combF ∷ a → Forest a → Forest a → Forest a
combF a fa fb = (Node a fa) : fb


pruneDff ∷ Int → Forest Vertex → Forest Vertex
pruneDff n f = runM n False (chop f) where  
  chop []               = return []
  chop (Node v f' : us) = let us'  = chop us
                              f''  = chop f'
                              nvis = visit v >> liftM2 (combF v) f'' us'
                          in  ifM (visited v) us' nvis


-- | Depth first search ignoring nodes
--   which do not fulfill a given predicate.
--   This is useful for instance to generate
--   all degree two bridges ins cyclic graphs
-- dfsFiltered ∷ Graph → (Vertex → Bool) → Forest Vertex
-- dfsFiltered g p = let vs  = filter p $ vertices g
--                       ns  = filter p . neighbors g
--                       gen v = Node v $ fmap gen (ns v)
--                   in  pruneDfs (order g) $ fmap gen vs
-- 
