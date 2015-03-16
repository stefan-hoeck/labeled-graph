{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Graph.Simple.Graph (
-- * Types and data types
   Graph, edges, edgeList

-- * Graph constructors
, null, empty, complete, chain, complement, fromEdges, fromList

-- * Graph properties
, order, size, isNull, isEmpty, isTrivial
, minDegree, maxDegree, vertices

-- * Vertex properties
, degree, degrees, neighbors, adjacent, isPendant, isIsolate
-- , reachable

-- * Edge properties
, isPendantEdge, edgeIn, edgesAt

-- * Subgraphs
, filterE, filterV, inducedSubgraph, connectedSubgraphs
, isConnected

-- * Cycles
, cyclicVertices

-- * Searches
, dfs, bfs, reachable, paths, pathsN
, pathTree, pathTreeN, treeToPaths, treeToMaxPaths
, cycles, cyclesN


-- * Pretty printing
, pretty, prettyShow

, module Data.Graph.Simple.Vertex
, module Data.Graph.Simple.Edge
) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Control.Monad.ST (runST)
import Data.Foldable (forM_, toList, foldMap)
import Data.Graph.Simple.Edge
import Data.Graph.Simple.Util
import Data.Graph.Simple.Vertex
import Data.List (sort, intercalate)
import Data.Tree (Tree(..), Forest)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Safe.Foldable (minimumMay, maximumMay)

import qualified Data.BitSet.Word as BS
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MVU

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
--   most cases, so it is defacto O(1).
degree ∷ Graph → Vertex → Int
degree g = length . neighbors g

degrees ∷ Graph → [Int]
degrees g = fmap (degree g) (vertices g)


isPendant ∷ Graph → Vertex → Bool
isPendant g v = (degree g v) == 1


isIsolate ∷ Graph → Vertex → Bool
isIsolate g v = (degree g v) == 0


-- | The vertices bound via a single edge to a given vertex v
neighbors ∷ Graph → Vertex → [Vertex]
neighbors g v = conList g V.! unVertex v

connectedSubgraphs ∷ Graph → [Graph]
connectedSubgraphs g = fmap (inducedSubgraph g . toList) $ dff g
                

reachable ∷ Graph → Vertex → [Vertex]
reachable g v = foldMap toList $ dfs g [v]

isConnected ∷ Graph → Bool
isConnected g = (length $ reachable g 0) == order g

-- | Depth first search implementation similar to
--   the one found in Data.Graph of the containers
--   package.
dfs ∷ Graph → [Vertex] → Forest Vertex
dfs g vs = pruneDfs (order g) (map (generate g) vs)

dff ∷ Graph → Forest Vertex
dff g = dfs g (vertices g)

generate ∷ Graph → Vertex → Tree Vertex
generate g v  = Node v $ map (generate g) (neighbors g v)


cyclicVertices ∷ Graph → [Vertex]
cyclicVertices g = let cs = cvs g
                   in filter ((cs UV.!) . unVertex) (vertices g)

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
pathTreeN n g v = head $ prunePaths n (order g) [generate g v]


-- | Returns all paths starting from a given vertex
paths ∷ Graph → Vertex → [[Vertex]]
paths g = treeToPaths . pathTree g


-- | Returns all paths of a given length
--   starting from a given vertex
pathsN ∷ Int → Graph → Vertex → [[Vertex]]
pathsN n g = filter ((n==) . length) . treeToMaxPaths . pathTreeN n g

cycles ∷ Graph → Vertex → [[Vertex]]
cycles g v = fmap (v:) . filter (keepCycle g v) $ paths g v

cyclesN ∷ Int → Graph → Vertex → [[Vertex]]
cyclesN n g v = fmap (v:) . filter (keepCycle g v) $ pathsN n g v

keepCycle ∷ Graph → Vertex → [Vertex] → Bool
keepCycle g v (v':t@(_:_:_)) | adjacent g v v'  = keep t
                          where keep (v'':_:[]) = v'' > v'
                                keep (_:t')     = keep t'
                                keep _          = error "not possible"
keepCycle _ _ _                                 = False


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

pruneDfs ∷ Int → Forest Vertex → Forest Vertex
pruneDfs n ts = runM n False (chop ts)
  where  chop []       = return []
         chop (Node v ts' : us) = do vis ← visited v
                                     if vis 
                                     then 
                                       chop us
                                     else do 
                                       visit v
                                       as ← chop ts'
                                       bs ← chop us
                                       return (Node v as : bs)

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

cvs ∷ Graph → UV.Vector Bool
cvs g = let o = order g
        in
        runST $ do vs ← MVU.replicate o False     -- visited vertices
                   mk ← MVU.replicate o (0 ∷ Int) -- vertices marked as cycle endpoints
                   cs ← MVU.replicate o False     -- vertices in cycles

                   let visited'  = unsafeReadVU vs
                   let visit' v  = unsafeWriteVU vs v True
                   let marked'   = unsafeReadVU mk
                   let mark' v   = unsafeModVU mk v (+1)
                   let unmark' v = unsafeWriteVU mk v 0
                   let cycle' v  = unsafeWriteVU cs v True
                   let ns v      = neighbors g v
                   let ns' p v   = filter (p /=) $ ns v

                   let check' p v = do vis ← visited' v
                                       if vis 
                                       then do
                                         mark' v            
                                         return 1
                                       else do
                                         visit' v
                                         is ← mapM (check' v) $ ns' p v
                                         let s = sum is
                                         if s > 0    -- we are within a cycle
                                         then do
                                           cycle'  v
                                           m ← marked' v
                                           if m > 0  -- we are at the end of some cycles
                                           then do
                                             unmark' v
                                             return $ s-m
                                           else return s
                                         else return s

                   let check v = do vis ← visited' v
                                    unless vis $ mapM_ (check' v) (ns v)

                   mapM_ check $ vertices g

                   UV.unsafeFreeze cs
