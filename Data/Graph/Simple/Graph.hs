{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Graph.Simple.Graph (
-- * Types and data types
   Graph, edges, edgeList

-- * Graph constructors
, null, empty, complete, chain, complement, fromEdges, fromList
, fromList', fromEdges'

-- * Graph properties
, order, size, isNull, isEmpty, isTrivial, isComplete
, minDegree, maxDegree, vertices

-- * Vertex properties
, degree, degrees, degreesV, neighbors, adjacent, isPendant, isIsolate
-- , reachable

-- * Edge properties
, isPendantEdge, edgeIn, edgesAt

-- * Subgraphs
, filterE, filterV, inducedSubgraph, connectedSubgraphs
, isConnected

-- * Searches
, dfs, dff, dfsFiltered, bfs, reachable, paths, pathsN
, pathTree, pathTreeN, treeToPaths, treeToMaxPaths, shortestPaths
, generateTree, generateForest, cycleForest


-- * Pretty printing
, pretty, prettyShow


-- * Low level
, pruneDfs

, module Data.Graph.Simple.Vertex
, module Data.Graph.Simple.Edge
) where

import Control.DeepSeq (NFData)
import Control.Monad (liftM2)
import Control.Monad.ST (runST)
import Data.Foldable (forM_, toList)
import Data.Graph.Simple.Edge
import Data.Graph.Simple.Util
import Data.Graph.Simple.Vertex
import Data.List (sort, intercalate)
import Data.Tree (Tree(..), Forest)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Safe.Foldable (minimumMay, maximumMay)

-- import qualified Data.BitSet.Word as BS
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- ** Graphs ** --
--

type ConList = V.Vector [Vertex]
  
data Graph = Graph {
  conList ∷ ConList
, edges   ∷ Edges
} deriving (Eq, Generic)

instance NFData Graph
  
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

-- | Builds a graph from a list of edges
--
-- The order is take from the largest vertex connected by an edge
fromList' ∷ [Edge] → Graph
fromList' = fromEdges' . edgesFromList


-- | Builds a graph of the given order from a list of edges
fromEdges ∷ Int → Edges → Graph
fromEdges o es = Graph (edgesToConList o es') es
    where es'  = unEdges es

-- | Builds a graph from a list of edges
--
-- The order is take from the largest vertex connected by an edge
fromEdges' ∷ Edges → Graph
fromEdges' es = let mvMay   = fmap unVertex $ maximumV es
                    makeG v = fromEdges (v+1) es
                in  maybe null makeG mvMay


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

-- | True if the graph is complete (every vertex is adjacent
--   to every other vertex)
isComplete ∷ Graph → Bool
isComplete g = let o = order g
               in  size g == (o * (o-1)) `div` 2


-- | The order (number of vertices) of a graph
order ∷ Graph → Int
order =  V.length . conList


-- | The size (number of edges) of a graph
size ∷ Graph → Int
size = edgesSize . edges


vertices ∷ Graph → [Vertex]
vertices g | isNull g  = []
           | otherwise = [minVertex.. vertex $ (order g - 1)]


minDegree ∷ Graph → Maybe Int
minDegree = minimumMay . degrees


maxDegree ∷ Graph → Maybe Int
maxDegree = maximumMay . degrees


-- * Vertex properties * --
--

-- | True if the two vertices are adjacent (bound via a
--   single edge).
--
--   O(n) where n is the number of neighbors
--   of the second vertex. For molecules, n ≤ 4 in
--   most cases, so it is de-facto O(1).
adjacent ∷ Graph → Vertex → Vertex → Bool
adjacent g v1 v2 = v2 `elem` neighbors g v1


-- | The degree of a vertex v.
--   This is equal to the number of vertices directly
--   bound to v
--
--   O(n) where n is the number of neighbors
--   of the second vertex. For molecules, n ≤ 4 in
--   most cases, so it is de-facto O(1).
degree ∷ Graph → Vertex → Int
degree g = length . neighbors g

degrees ∷ Graph → [Int]
degrees g = fmap (degree g) (vertices g)

degreesV ∷ Graph → UV.Vector Int
degreesV g = UV.generate (order g) (degree g . vertex)


isPendant ∷ Graph → Vertex → Bool
isPendant g v = (degree g v) == 1


isIsolate ∷ Graph → Vertex → Bool
isIsolate g v = (degree g v) == 0


-- | The vertices bound via a single edge to a given vertex v
neighbors ∷ Graph → Vertex → [Vertex]
neighbors g v = conList g V.! unVertex v

-- | Creates the connected components of the given graph
--
--   Since the numbering of the subgraphs might be different
--   compared to the original graph, a mapping from the new to
--   the old numbering is given with each graph.
connectedSubgraphs ∷ Graph → [(Graph, V.Vector Vertex)]
connectedSubgraphs g = case dff g of
                         []  → []
                         [_] → [(g, V.fromList $ vertices g)]
                         f   → fmap (inducedSubgraph g . toList) f
                

reachable ∷ Graph → Vertex → [Vertex]
reachable g v = foldMap toList $ dfs g [v]


-- | True if the graph is connected
--
--   Runs in O(|V|+|E|) time
isConnected ∷ Graph → Bool
isConnected g = case dff g of
                     []  → True
                     [_] → True
                     _   → False


-- | Depth first search implementation similar to
--   the one found in Data.Graph of the containers
--   package.
--
--   Runs in O(|V|+|E|) time
dfs ∷ Graph → [Vertex] → Forest Vertex
dfs g vs = pruneDfs (order g) (fmap (generateTree g) vs)


-- | Depth first search ignoring nodes
--   which do not fulfill a given predicate.
--   This is useful for instance to generate
--   all degree two bridges ins cyclic graphs
dfsFiltered ∷ Graph → (Vertex → Bool) → Forest Vertex
dfsFiltered g p = let vs  = filter p $ vertices g
                      ns  = filter p . neighbors g
                      gen v = Node v $ fmap gen (ns v)
                  in  pruneDfs (order g) $ fmap gen vs


-- | Depth first search forest containing all the
--   vertices of a graph.
--
--   From the dff we can directly derive the connected components
--   of a graph
--
--   Runs in O(|V|+|E|) time
dff ∷ Graph → Forest Vertex
dff g = dfs g (vertices g)


generateTree ∷ Graph → Vertex → Tree Vertex
generateTree g v  = Node v $ map (generateTree g) (neighbors g v)

generateForest ∷ Graph → Forest Vertex
generateForest g = fmap (generateTree g) $ vertices g


bfs ∷ Graph → Vertex → [[Vertex]]
bfs g v = runM (order g) False $ bfs' [v]
    where bfs' []      = return []
          bfs' vs      = do vs'  ← pruneBfs vs
                            vs'' ← bfs' (vs' >>= neighbors g)
                            return $ vs' : vs''


-- | Creates a tree of all paths starting from the vertex
--   given
pathTree ∷ Graph → Vertex → Tree Vertex
pathTree = pathTreeN (-1)


-- | Creates a tree of paths up to the given number of
--   vertices starting from the vertex given
pathTreeN ∷ Int → Graph → Vertex → Tree Vertex
pathTreeN n g v = head $ prunePaths n (order g) [generateTree g v]


-- | Returns all paths starting from a given vertex
paths ∷ Graph → Vertex → [[Vertex]]
paths g = treeToPaths . pathTree g


cycleForest ∷ Graph → Forest Vertex
cycleForest g = pruneDangling (order g) $ generateForest g

-- | Returns a shortest path to each connected
--   vertex starting from a given vertex
shortestPaths ∷ Graph → Vertex → [[Vertex]]
shortestPaths g v = runM (order g) False $ do visit v
                                              run [[v]]
  where run   []   = return []
        run   vss  = do res ← fmap concat $ traverse next vss
                        add ← run res
                        return $ vss ++ add
        next   vs@(h:_) = fmap concat $ traverse (single vs) $ neighbors g h
        next   []       = error "oops"
        single vs v'    = do vis ← visited v'
                             if vis then return []
                                    else do  visit v'
                                             return [v':vs]

-- | Returns all paths of a given length
--   starting from a given vertex
pathsN ∷ Int → Graph → Vertex → [[Vertex]]
pathsN n g = filter ((n==) . length) . treeToMaxPaths . pathTreeN n g


treeToPaths ∷ Tree a → [[a]]
treeToPaths = ttp []
    where ttp ps (Node v cs) = let ps' = v:ps
                               in ps' : (cs >>= ttp ps')

treeToMaxPaths ∷ Tree a → [[a]]
treeToMaxPaths = ttp []
    where ttp ps (Node v []) = [v:ps]
          ttp ps (Node v cs) = let ps' = v:ps
                               in cs >>= ttp ps'

--                   
-- * Edge properties *

edgeList ∷ Graph → [Edge]
edgeList = unEdges . edges


isPendantEdge ∷ Graph → Edge → Bool
isPendantEdge g e = let (x, y) = (edgeX e, edgeY e)
                    in edgeIn g e && (isPendant g x || isPendant g y)


edgeIn ∷ Graph → Edge → Bool
edgeIn g e = adjacent g (edgeX e) (edgeY e)


edgesAt ∷ Graph → Vertex → [Edge]
edgesAt g v = fmap (edge v) $ neighbors g v



-- * Subgraphs * --
--

-- | Returns a subgraph of the given graph, keeping
--   only the vertices given and the edges involving
--   those vertices.
--
--   Note that the numbering of vertices will be adjusted
--   in the new graph, therefore a mapping from the new
--   to the old numbering is returned together with the
--   new graph
inducedSubgraph ∷ Graph → [Vertex] → (Graph, V.Vector Vertex)
inducedSubgraph g vs = let bm = boolMap (order g) vs
                       in  filterV ((bm UV.!) . unVertex) g


-- | Returns a subgraph of the given graph, keeping
--   only the vertices fulfilling the given predicate
--
--   Note that the numbering of vertices will be adjusted
--   in the new graph, therefore a mapping from the new
--   to the old numbering is returned together with the
--   new graph
filterV ∷ (Vertex → Bool) → Graph → (Graph, V.Vector Vertex)
filterV p g      = (fromEdges (length vs) es, V.fromList vs)
    where es     = unsafeEdges . concatMap adjE $ edgeList g

          vs     = filter p (vertices g)

          -- TODO Go from Map to Vector with dummy elements here
          vmap   = M.fromAscList $ vs `zip` [0..]

          adjV v = vmap M.! v

          adjE e = case (edgeX e, edgeY e) of
                     (x, y) | p x && p y → [unsafeEdge (adjV x) (adjV y)]
                            | otherwise  → []


filterE ∷ (Edge → Bool) → Graph → Graph
filterE p g = fromEdges (order g) . filterEdges p $ edges g


-- * Graph Visualisation * --
--
pretty ∷ (Vertex → String) → (Edge → String) → Graph → String
pretty fv fe g = intercalate "\n" $ zipWith (++) elbls vlbls

  where elbls = rightPad ' ' $ fmap fe (edgeList g) ++ eadd
        vlbls = rightPad ' ' $ fmap (("   " ++) . fv) (vertices g) ++ vadd
        eadd  = replicate (order g - size g) ""
        vadd  = replicate (size g - order g) ""

prettyShow ∷ Graph → String
prettyShow = pretty show show


----------------------------------------------------------------------
-- Algorithms

edgesToConList ∷ Int → [Edge] → ConList
edgesToConList o es = runST $ do
  v ← V.unsafeThaw $ V.replicate o []

  let setEdge e = let (x, y) = (edgeX e, edgeY e)
                  in unsafeModV v x (y :) >>
                     unsafeModV v y (x :)

  forM_ es setEdge
  forM_ [0.. o-1] $ \i → unsafeMod v i sort

  V.unsafeFreeze v


combF ∷ a → Forest a → Forest a → Forest a
combF a fa fb = (Node a fa) : fb

pruneDfs ∷ Int → Forest Vertex → Forest Vertex
pruneDfs n f = runM n False (chop f) where  
  chop []               = return []
  chop (Node v f' : us) = let us'  = chop us
                              f''  = chop f'
                              nvis = visit v >> liftM2 (combF v) f'' us'
                          in  ifM (visited v) us' nvis


pruneDangling ∷ Int → Forest Vertex → Forest Vertex
pruneDangling n f = runM n False (chop (-1) (-1) f) where  
  chop _ _ []                 = return []
  chop p p' (Node v f' : us) = let v'   = unVertex v
                                   f''  = chop p' v' f'
                                   us'  = chop p  p' us
                                   keep = fmap (combF v []) us'

                                   vis  = if p == (-1) || p == v'
                                            then us'
                                            else keep
                                   nvis = visit v >> liftM2 (combF v) f'' us'
                                    
                               in  ifM (visited v) vis nvis


pruneBfs ∷ [Vertex] → SetM e Bool [Vertex]
pruneBfs []    = return []
pruneBfs (h:t) = do vis ← visited h
                    if vis 
                      then pruneBfs t
                      else do visit h
                              t' ← pruneBfs t
                              return $ h:t'


prunePaths ∷ Int → Int → Forest Vertex → Forest Vertex
prunePaths d n ts = runM n False $ chop d ts
  where  chop _ []       = return []
         chop 0 _        = return []
         chop d' (Node v ts' : us) = do bs  ← chop d' us
                                        vis ← visited v
                                        if vis 
                                        then 
                                          return bs 
                                        else do 
                                          visit v
                                          as ← chop (d'-1) ts'
                                          unvisit v
                                          return (Node v as : bs)
