{-# LANGUAGE DeriveGeneric #-}

module Data.Graph.Simple.LGraph (
  LGraph(graph, vlabels, elabels)

, fromLists, fromGraph, null, empty

, isNull, isEmpty, isTrivial, order, size, vertices
, minDegree, maxDegree
, vlabel, elabel

, mapE, mapV

, filterE

-- * Pretty printing
, pretty, prettyShow

, module Data.Graph.Simple.Vertex
, module Data.Graph.Simple.Edge
) where

import Control.DeepSeq (NFData)
import Data.Foldable (Foldable, foldMap, foldr)
import Data.Graph.Simple.Edge
import Data.Graph.Simple.Vertex
import Data.Maybe (fromJust)
import Data.Traversable (Traversable, traverse)
import GHC.Generics (Generic)
import Prelude hiding (null, foldr)

import qualified Data.Map as M
import qualified Data.Graph.Simple.Graph as G
import qualified Data.Vector as V

data LGraph e v = LGraph {
  graph   ∷ G.Graph
, vlabels ∷ V.Vector v
, elabels ∷ M.Map Edge e
} deriving (Eq, Show, Generic)

instance (NFData e, NFData v) ⇒ NFData (LGraph e v)

instance Functor (LGraph e) where
  fmap = mapV

instance Foldable (LGraph e) where
  foldMap f = foldMap f . vlabels
  foldr f b = foldr f b . vlabels

instance Traversable (LGraph e) where
  traverse f (LGraph g vs es) = let vs' = traverse f vs
                                in fmap (\x → LGraph g x es) vs'


fromGraph ∷ (Vertex → v) → (Edge → e) → G.Graph → LGraph e v
fromGraph fv fe g = LGraph g vs es 
    where vs = V.generate (G.order g) (fv . vertex)
          es = M.fromList $ fmap (\e → (e, fe e)) (G.edgeList g)

fromLists ∷ [v] → [(Edge, e)] → LGraph e v
fromLists vs es = LGraph g (V.fromList vs) (M.fromList es)
    where g = G.fromList (length vs) (fmap fst es)

-- | The null graph
null ∷ LGraph v e
null = LGraph G.null V.empty M.empty


-- | An empty graph (a graph without edges) with n vertices
empty ∷ Int → (Vertex → v) → LGraph e v
empty o fv   = LGraph G.null vs M.empty
    where vs = V.generate o (fv . vertex)

-- | Extract the label at a given vertex
vlabel ∷ LGraph e v → Vertex → v
vlabel g v = (vlabels g) V.! (unVertex v)


-- | Extract the label at a given edge
elabel ∷ LGraph e v → Edge → Maybe e
elabel g e = M.lookup e (elabels g)


-- * Basic Graph properties * --
--

-- | True if the graph has no vertices (order == 0)
isNull ∷ LGraph e v → Bool
isNull = G.isNull . graph


-- | True if the graph has no edges (size == 0)
isEmpty ∷ LGraph e v → Bool
isEmpty = G.isEmpty . graph


-- | True if the graph has only one vertex
isTrivial ∷ LGraph e v → Bool
isTrivial = G.isTrivial . graph


-- | The order (number of vertices) of a graph
order ∷ LGraph e v → Int
order =  G.order . graph


-- | The size (number of edges) of a graph
size ∷ LGraph e v → Int
size = G.size . graph


vertices ∷ LGraph e v → [Vertex]
vertices = G.vertices . graph


minDegree ∷ LGraph e v → Maybe Int
minDegree = G.minDegree . graph


maxDegree ∷ LGraph e v → Maybe Int
maxDegree = G.maxDegree . graph


-- * Modifying Graphs * --
--

mapE ∷ (e → e') → LGraph e v → LGraph e' v
mapE f (LGraph g vs es) = LGraph g vs (fmap f es)

mapV ∷ (v → v') → LGraph e v → LGraph e v'
mapV f (LGraph g vs es) = LGraph g (fmap f vs) es


filterE ∷ (e → Bool) → LGraph e v → LGraph e v
filterE p (LGraph g vs es) = LGraph g' vs es'
    where g'  = G.fromEdges (G.order g) $ unsafeEdges (M.keys es')
          es' = M.filter p es

-- * Pretty Printing * --
--

pretty ∷ ((Vertex,v) → String) → ((Edge,e) → String) → LGraph e v → String
pretty fv fe g = G.pretty dispV dispE $ graph g
    where dispV v = fv (v, vlabel g v)
          dispE e = fe (e, fromJust $ elabel g e)

prettyShow ∷ (Show e, Show v) ⇒ LGraph e v → String
prettyShow = pretty disp disp
    where disp (v,vl) = show v ++ ": " ++ show vl
