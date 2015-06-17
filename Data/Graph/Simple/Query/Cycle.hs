module Data.Graph.Simple.Query.Cycle (
  cyclicEdges, cyclicVertices, cyclicSubgraph
, cyclomaticNumber, numberOfCycles
, cycleBases
, cycles
, allCycles
, isElementary
) where

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Functor (($>), void)
import Data.Graph.Simple.Graph hiding (null)
import Data.Graph.Simple.Util
import Data.Graph.Simple.Query.Dfs
import Data.Graph.Simple.Query.Bfs
import Data.STRef (newSTRef, modifySTRef', readSTRef)
import Data.Vector ((!))

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

-- | Calculates the cyclomatic number μ(G) of a graph G
--   in O(|V|+|E|) time
--
--   The cyclomatic number is equal to the dimension
--   of the cyclic vector space of G and can be used to
--   efficiently estimate the complexity of generating
--   all cycles of a graph.
--
--   The total number of cycles in a graph equals 2^μ(G).
cyclomaticNumber ∷ Graph → Int
cyclomaticNumber g = let c = length $ dfs g (vertices g)
                     in  size g - order g + c


-- | Calculates the number of cycles in a graph
--   in O(|V|+|E|) time.
numberOfCycles ∷ Graph → Integer
numberOfCycles = (2 ^) . cyclomaticNumber


-- | Calculates the cyclic edges of a graph
cyclicEdges ∷ Graph → [Edge]
cyclicEdges = snd . cvs


-- | Returns a list of vertices which are part of
--   of some cycle in the graph
cyclicVertices ∷ Graph → [Vertex]
cyclicVertices g = let cs = fst $ cvs g
                   in  filter ((cs VU.!) . unVertex) (vertices g)


-- | Returns a subgraph containing only cylic edges.
--   The order of the graph will not change, so non-cyclic
--   vertices in the original graph will be isolate in
--   the returned subgraph.
cyclicSubgraph ∷ Graph → Graph
cyclicSubgraph g = fromList (order g) (cyclicEdges g)


-- | Produces a set of disconnected, non-minimal cycle bases
--   one for each disonnected cycle system in the graph.
--
--   This method does not produce a particular cycle base
--   and is useful mainly to generate the complete set
--   of (elementary) cycles of a graph.
cycleBases ∷ Graph → [[Edges]]
cycleBases g = let g'    = cyclicSubgraph g
                   paths = bfPaths g' (vertices g')
               in  filter (not . null) $ fmap (cycleBase' g') paths


-- | True if the given list of edges represents an elementary
--   cycle
isElementary ∷ Edges → Bool
isElementary = run . fmap (\e → (edgeX e, edgeY e)) . unEdges
  where run []                                 = False
        run [(a,b)]                            = a == b
        run ((a,b):(c,d):t) | a /= b && a == c = run $ insert (b,d) t
                            | otherwise        = False

        insert p t = let (a,b) = span (p >=) t
                     in  a ++ (p : b)
        

-- | Enumerates all elementary cycles of a graph
cycles ∷ Graph → [Edges]
cycles = filter isElementary . allCycles


-- | Enumerates all cycles of a graph
allCycles ∷ Graph → [Edges]
allCycles = concatMap fromBase . cycleBases
  where fromBase []    = []
        fromBase (h:t) = let merge = edgesSymmetricDifference h
                             rest  = fromBase t
                         in  h : rest ++ fmap merge rest
        

----------------------------------------------------------------------
-- Algorithms

cvs ∷ Graph → (VU.Vector Bool, [Edge])
cvs g = let o = order g
        in
        runST $ do vs ← MVU.replicate o False     -- visited vertices
                   mk ← MVU.replicate o (0 ∷ Int) -- vertices marked as cycle endpoints
                   cs ← MVU.replicate o False     -- vertices in cycles
                   es ← newSTRef []               -- list of cyclic edges

                   let visited'   = unsafeReadVU vs
                   let visit'   v = unsafeWriteVU vs v True
                   let marked'    = unsafeReadVU mk
                   let mark'    v = unsafeModVU mk v (+1)
                   let unmark'  v = unsafeWriteVU mk v 0
                   let cycle'   v = unsafeWriteVU cs v True
                   let ns'    p   = filter (p /=) . neighbors g
                   let addE   p v = modifySTRef' es (edge v p :)

                   let check' p v = do vis ← visited' v
                                       if vis 
                                         then mark' v >> addE p v $> 1
                                         else do
                                           visit' v
                                           s ← fmap sum $ mapM (check' v) $ ns' p v
                                           if s > 0    -- we are within a cycle
                                             then do
                                               cycle'  v
                                               m ← marked' v
                                               let res = s - 2*m
                                               if m > 0  -- we are at the end of some cycles
                                                 then do
                                                   unmark' v
                                                   when (res > 0) $ addE p v
                                                   return $ res
                                                 else addE p v >> return s
                                              else return s

                   let check v    = unlessM (visited' v) (void $ check' v v)


                   mapM_ check $ vertices g
 
                   (,) <$> VU.unsafeFreeze cs <*> readSTRef es


-- Takes a graph and a breadth first spanning tree (list of paths)
-- and returns a cycle base built from the edges missing from this
-- tree.
cycleBase' ∷ Graph → [[Vertex]] → [Edges]
cycleBase' g vss = let pairs = fmap (\vs@(v:_) → (unVertex v, reverse vs)) vss
                       m     = partMap (order g) [] pairs
                       keep  = not . null . (m !) . edgeXInt
                       es    = filterEdges keep $ edges g
                       cyc e = toCycle e (m ! edgeXInt e) (m ! edgeYInt e)
                   in  fmap cyc $ unEdges $ nonSpanningEdges es vss

spanningEdges ∷ [[Vertex]] → Edges
spanningEdges = let toEdge (a:b:_) = [edge a b]
                    toEdge _       = []
                in  edgesFromList . concatMap toEdge

nonSpanningEdges ∷ Edges → [[Vertex]] → Edges
nonSpanningEdges es vs = es `edgesDifference` spanningEdges vs


toCycle ∷ Edge → [Vertex] → [Vertex] → Edges
toCycle e as@(_:ta@(a:_)) bs@(_:tb@(b:_)) 
        | a == b    = toCycle e ta tb
        | otherwise = edgesFromList $ e : vs2es as ++ vs2es bs
toCycle e a      b  = error $ "oops in $toCycle$: " ++ show e ++ ", " ++ show a ++ ", " ++ show b

-- | Takes a list of vertices and creates edges between
--   successors in this list.
vs2es ∷ [Vertex] → [Edge]
vs2es []       = []
vs2es vs@(_:t) = zipWith edge vs t
