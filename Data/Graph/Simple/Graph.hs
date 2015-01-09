module Data.Graph.Simple.Graph (
-- * Types and data types
  Graph, edges, edgeList, fromList

-- * Graph constructors
, null, empty, complete, chain, complement

-- * Graph properties
, order, size, isNull, isEmpty, isTrivial
, minDegree, maxDegree, vertices

-- * Vertex properties
, degree, neighbors, adjacent, isPendant, isIsolate
, reachable

-- * Edge properties
, isPendantEdge, edgeIn

-- * Subgraphs
-- , inducedSubgraph, filterVertices
) where

import Control.Monad ((>>=), (>>), return)
import Control.Monad.ST (runST)
import Data.Bool (Bool(..), (&&), (||), not)
import Data.Eq ((==))
import Data.Foldable (forM_, elem)
import Data.Function ((.), ($))
import Data.Functor (fmap)
import Data.Graph.Simple.Vertex
import Data.Graph.Simple.Edge
import Data.Graph.Simple.Util
import Data.Int (Int)
import Data.List (sort, length, unlines, filter)
import Data.Maybe (Maybe(..))
import Data.Ord ((<))
import GHC.Num ((-))
import Text.Show (Show, show)
import Safe.Foldable (minimumMay, maximumMay)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.List as L

-- ** Graphs ** --
--

type ConList = V.Vector Vertices
  
data Graph = Graph {
  conList ∷ ConList
, edges   ∷ [Edge]
}
  
instance Show Graph where
  show = unlines . fmap show . edgeList


-- * Graph Construction * --
--

-- | The null graph
null ∷ Graph
null = empty 0


-- | An empty graph (a graph without edges) with n vertices
empty ∷ Int → Graph
empty n = Graph (V.replicate n []) []


-- | Builds a graph from a list of edges
fromList ∷ Int → [Edge] → Graph
fromList o = fromSortedList o . sortedUnique


-- | Builds a graph from a list of edges
fromSortedList ∷ Int → [Edge] → Graph
fromSortedList o es = Graph (edgesToConList o es) es

fromConList ∷ ConList → Graph
fromConList cl = Graph cl calcEs
    where calcEs = [0 .. V.length cl - 1] >>= atV . vertex
          atV v  = let ns = cl V.! (unVertex v)
                   in fmap (v `edge`) $ filter (v <) ns

complete ∷ Int → Graph
complete o = fromSortedList o $ completeEdges o

 
chain ∷ Int → Graph
chain o = fromSortedList o $ chainEdges o
 
 
complement ∷ Graph → Graph
complement g = let o = order g
               in fromSortedList o 
                  . filter (not . edgeIn g) 
                  $ completeEdges o
 
-- * Basic Graph properties * --
--

-- | True if the graph has no vertices (order == 0)
isNull ∷ Graph → Bool
isNull = (0 ==) . order


-- | True if the graph has no edges (size == 0)
isEmpty ∷ Graph → Bool
isEmpty = L.null . edges


-- | True if the graph has only one vertex
isTrivial ∷ Graph → Bool
isTrivial = (1 ==) . order


-- | The order (number of vertices) of a graph
order ∷ Graph → Int
order =  V.length . conList


-- | The size (number of edges) of a graph
size ∷ Graph → Int
size = length . edges


vertices ∷ Graph → Vertices
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
neighbors ∷ Graph → Vertex → Vertices
neighbors g v = (conList g) V.! (unVertex v)
                

reachable ∷ Graph → Vertex → Vertices
reachable = dfs (:) []

dfs ∷ (Vertex → a → a) → a → Graph → Vertex → a
dfs f a g v = runST $ do
    vs ← MVU.replicate (order g) False
    run a [v] vs

  where run a' []    _  = return a'
        run a' (h:t) vs = do visited ← unsafeReadVU vs h
                             if visited
                               then run a' t vs
                               else do unsafeWriteVU vs h True
                                       a'' ← run (f h a') (neighbors g h) vs
                                       run a'' t vs

-- * Edge properties *
--

edgeList ∷ Graph → [Edge]
edgeList = edges


isPendantEdge ∷ Graph → Edge → Bool
isPendantEdge g e = edgeIn g e &&
                    (isPendant g (edgeX e) || isPendant g (edgeY e))

edgeIn ∷ Graph → Edge → Bool
edgeIn g e = adjacent g (edgeX e) (edgeY e)



-- * Subgraphs * --
--

-- inducedSubgraph ∷ Vertices → Graph → Graph
-- inducedSubgraph vs g = fromEdges newOrder
--                      . normalizeEdges 
--                      . filterEdges inV 
--                      $ edges g
-- 
--   where inV e    = map UV.! (edgeXInt e) && map UV.! (edgeYInt e)
--         newOrder = length vs
--         map      = runST $ do
--                      map' ← MVU.replicate (order g) False
--                      forM_ vs $ \v → unsafeWriteVU map' v True
--                      UV.unsafeFreeze map'
--        
-- 
-- filterVertices ∷ (Vertex → Bool) → Graph → Graph
-- filterVertices p g = inducedSubgraph (filter p $ vertices g) g

-- * Helper functions * --
--

edgesToConList ∷ Int → [Edge] → ConList
edgesToConList o es = runST $ do
  v ← V.unsafeThaw $ V.replicate o []

  let setEdge e = let (x, y) = (edgeX e, edgeY e)
                  in unsafeModV v x (y :) >>
                     unsafeModV v y (x :)

  forM_ es setEdge
  forM_ [0.. o-1] $ \i → unsafeMod v i sort

  V.unsafeFreeze v
