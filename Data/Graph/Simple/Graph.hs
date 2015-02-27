{-# LANGUAGE RankNTypes #-}

module Data.Graph.Simple.Graph (
-- * Types and data types
   Graph, edges, edgeList

-- * Graph constructors
, null, empty, complete, chain, complement, fromEdges, fromList

-- * Graph properties
, order, size, isNull, isEmpty, isTrivial
, minDegree, maxDegree, vertices

-- * Vertex properties
, degree, neighbors, adjacent, isPendant, isIsolate
-- , reachable

-- * Edge properties
, isPendantEdge, edgeIn

-- * Subgraphs
, filterE, filterV, inducedSubgraph

-- * Searches
, dfs, reachable, connectedSubgraphs

, SetM(..), runG, visited, visit

, module Data.Graph.Simple.Vertex
, module Data.Graph.Simple.Edge
) where

import Control.Applicative (Applicative(..))
import Control.Monad.ST (ST, runST)
import Data.Foldable (forM_, toList, foldMap)
import Data.Graph.Simple.Edge
import Data.Graph.Simple.Util
import Data.Graph.Simple.Vertex
import Data.List (sort)
import Data.Tree (Tree(..), Forest)
import Prelude hiding (null)
import Safe.Foldable (minimumMay, maximumMay)

import qualified Data.BitSet.Word as BS
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MVU

-- ** Graphs ** --
--

type ConList = V.Vector [Vertex]
  
data Graph = Graph {
  conList ∷ ConList
, edges   ∷ Edges
}
  
instance Show Graph where
  show g =     "fromList "
            ++ show (order g)
            ++ " "
            ++ show (edgeList g)


-- * Graph Construction * --
--

-- | The null graph
null ∷ Graph
null = empty 0


-- | An empty graph (a graph without edges) with n vertices
empty ∷ Int → Graph
empty n = Graph (V.replicate n []) emptyEdges


-- | Builds a graph from a list of edges
fromList ∷ Int → [Edge] → Graph
fromList o = fromEdges o . edgesFromList


-- | Builds a graph of the given order from a list of edges
fromEdges ∷ Int → Edges → Graph
fromEdges o es = Graph (edgesToConList o es') es
    where es'  = unEdges es


-- fromConList ∷ ConList → Graph
-- fromConList cl = Graph cl calcEs
--     where vmax'  = vertex $ V.length cl
--           calcEs = unsafeEdges $ [0 .. vmax'] >>= atV
--           atV v  = let ns = cl V.! (unVertex v)
--                    in fmap (v `edge`) $ filter (v <) ns


-- | Returns the complete graph (edges between every pair of vertices)
--   of the given order
complete ∷ Int → Graph
complete o = fromEdges o $ completeEdges o


-- | Returns a connected chain graph (no cycles or branches)
--   of the given order
chain ∷ Int → Graph
chain o = fromEdges o $ chainEdges o


-- | Returns the complement of the given graph
--
--   The complement graph has an edge connecting two distinct vertices
--   v1 and v2 if and only if the original graph has no edge connecting
--   v1 and v2.
complement ∷ Graph → Graph
complement g = let o = order g
               in fromEdges o 
                  . filterEdges (not . edgeIn g) 
                  $ completeEdges o

-- * Basic Graph properties * --
--

-- | True if the graph has no vertices (order == 0)
isNull ∷ Graph → Bool
isNull = (0 ==) . order


-- | True if the graph has no edges (size == 0)
isEmpty ∷ Graph → Bool
isEmpty = edgesNull . edges


-- | True if the graph has only one vertex
isTrivial ∷ Graph → Bool
isTrivial = (1 ==) . order


-- | The order (number of vertices) of a graph
order ∷ Graph → Int
order =  V.length . conList


-- | The size (number of edges) of a graph
size ∷ Graph → Int
size = edgesSize . edges


vertices ∷ Graph → [Vertex]
vertices g = [minVertex.. vertex $ (order g - 1)]


minDegree ∷ Graph → Maybe Int
minDegree g = minimumMay . fmap (degree g) . vertices $ g


maxDegree ∷ Graph → Maybe Int
maxDegree g = maximumMay . fmap (degree g) . vertices $ g


-- * Vertex properties * --
--

-- | True if the two vertices are adjacent (bound via a
--   single edge).
--
--   O(n) where n is the number of neighbors
--   of the second vertex. For molecules, n ≤ 4 in
--   most cases, so it is de-facto O(1).
adjacent ∷ Graph → Vertex → Vertex → Bool
adjacent g v1 v2 = v2 `elem` (neighbors g v1)


-- | The degree of a vertex v.
--   This is equal to the number of vertices directly
--   bound to v
--
--   O(n) where n is the number of neighbors
--   of the second vertex. For molecules, n ≤ 4 in
--   most cases, so it is defacto O(1).
degree ∷ Graph → Vertex → Int
degree g = length . neighbors g


isPendant ∷ Graph → Vertex → Bool
isPendant g v = (degree g v) == 1


isIsolate ∷ Graph → Vertex → Bool
isIsolate g v = (degree g v) == 0


-- | The vertices bound via a single edge to a given vertex v
neighbors ∷ Graph → Vertex → [Vertex]
neighbors g v = (conList g) V.! (unVertex v)

connectedSubgraphs ∷ Graph → [Graph]
connectedSubgraphs g =
  fmap (inducedSubgraph g . toList) $ dfs g (vertices g)
                

reachable ∷ Graph → Vertex → [Vertex]
reachable g v = foldMap toList $ dfs g [v]

-- | Depth first search implementation similar to
--   the one found in Data.Graph of the containers
--   package.
dfs ∷ Graph → [Vertex] → Forest Vertex
dfs g vs = prune (order g) (map (generate g) vs)

generate ∷ Graph → Vertex → Tree Vertex
generate g v  = Node v $ map (generate g) (neighbors g v)

prune ∷ Int → Forest Vertex → Forest Vertex
prune n ts = run n (chop ts)

chop ∷ Forest Vertex → SetM s (Forest Vertex)
chop []       = return []
chop (Node v ts : us) = do vis ← visited v
                           if vis 
                           then chop us
                           else do visit v
                                   as ← chop ts
                                   bs ← chop us
                                   return (Node v as : bs)

-- * Edge properties *

edgeList ∷ Graph → [Edge]
edgeList = unEdges . edges


isPendantEdge ∷ Graph → Edge → Bool
isPendantEdge g e = edgeIn g e &&
                    (isPendant g (edgeX e) || isPendant g (edgeY e))


edgeIn ∷ Graph → Edge → Bool
edgeIn g e = adjacent g (edgeX e) (edgeY e)



-- * Subgraphs * --
--

-- | Returns a subgraph of the given graph, keeping
--   only the vertices given and the edges involving
--   those vertices.
--
--   Note that the numbering of vertices will be adjusted
--   in the new graph
inducedSubgraph ∷ Graph → [Vertex] → Graph
inducedSubgraph g vs = let bs = BS.fromList vs
                     in filterV (`BS.member` bs) g


-- | Returns a subgraph of the given graph, keeping
--   only the vertices fulfilling the given predicate
--
--   Note that the numbering of vertices will be adjusted
--   in the new graph
filterV ∷ (Vertex → Bool) → Graph → Graph
filterV p g      = fromEdges (length vs) es
    where es     = unsafeEdges . concatMap adjE $ edgeList g

          vs     = filter p (vertices g)

          vmap   = M.fromAscList $ vs `zip` [0..]

          adjV v = vmap M.! v

          adjE e = case (edgeX e, edgeY e) of
                     (x, y) | p x && p y → [unsafeEdge (adjV x) (adjV y)]
                            | otherwise  → []


filterE ∷ (Edge → Bool) → Graph → Graph
filterE p g = fromEdges (order g) . filterEdges p $ edges g


-- * Helper functions * --

edgesToConList ∷ Int → [Edge] → ConList
edgesToConList o es = runST $ do
  v ← V.unsafeThaw $ V.replicate o []

  let setEdge e = let (x, y) = (edgeX e, edgeY e)
                  in unsafeModV v x (y :) >>
                     unsafeModV v y (x :)

  forM_ es setEdge
  forM_ [0.. o-1] $ \i → unsafeMod v i sort

  V.unsafeFreeze v


-- Used to mark visited vertices in graph algorithms
newtype SetM s a = SetM { runSetM ∷ MVU.MVector s Bool → ST s a }

instance Functor (SetM s) where
  f `fmap` SetM v = SetM $ fmap f . v

instance Applicative (SetM s) where
  pure              = SetM . const . return
  SetM f <*> SetM a = SetM $ \v → f v <*> a v

instance Monad (SetM s) where
  return       = pure
  SetM v >>= f = SetM $ \s → do x ← v s
                                runSetM (f x) s

runG ∷ Graph → (forall s . SetM s a) → a
runG g = run $ order g


run ∷ Int → (forall s . SetM s a) → a
run n act = runST $ do v ← MVU.replicate n False
                       runSetM act v


visited ∷ Vertex → SetM s Bool
visited v = SetM $ \bs → MVU.read bs (unVertex v) 

visit ∷ Vertex → SetM s ()
visit v = SetM $ \bs → MVU.write bs (unVertex v) True