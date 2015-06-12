{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Graph.Simple.Edge (
  Edge, unEdge

, edgeX, edgeY, edgeXInt, edgeYInt
, edge, edgeMay, unsafeEdge, (<->)
, connects, edgeVertices
, edgesAdjacent, edgesSize
, edgesNull, filterEdges

,  Edges, unsafeEdges, unEdges, emptyEdges, edgesFromList
,  completeEdges, chainEdges
,  maximumV, minimumV
) where

import Control.DeepSeq (NFData)
import Data.Graph.Simple.Util (sortedUnique)
import Data.Graph.Simple.Vertex (Vertex, unVertex, maxVertex, vertex)
import Data.Monoid ((<>))
import Safe (maximumMay, minimumMay)

-- ** Edge ** --
--

-- | An edge in a simple graph
--
--   Internally, the edge is stored as an int using
--   modular arithmetik to extraxt the two vertices from
--   it. This leads to a very compact representation.
newtype Edge = Edge { unEdge ∷ Int }
  deriving (Eq, Ord, NFData)

instance Show Edge where
  show e = show (edgeXInt e) <> " <-> " <> show (edgeYInt e)


-- | Creates an edge from two vertices
--   Returns nothing if the two Vertices are equal
edgeMay ∷ Vertex → Vertex → Maybe Edge
edgeMay a b | a == b      = Nothing
            | a < b       = Just $ unsafeEdge a b
            | otherwise   = Just $ unsafeEdge b a

-- | Creates an edge from two vertices
--   Throws an exception if the two vertices are equal
edge ∷ Vertex → Vertex → Edge
edge a b | a == b         = error "The vertices of an edge must not be equal"
         | a < b          = unsafeEdge a b
         | otherwise      = unsafeEdge b a

vmod ∷ Int
vmod = 1 + (unVertex maxVertex)

-- | Creates an edge from two vertices
--   The two vertices must not be equal but this is not checked.
unsafeEdge ∷ Vertex → Vertex → Edge
unsafeEdge v1 v2 = Edge $ (unVertex v1) * vmod + (unVertex v2)

-- | Creates an edge from tow integers
--
--   Throws an error if the integers are equal or
--   do not represent valid vertices.
(<->) ∷ Int → Int → Edge
x <-> y = edge (vertex x) (vertex y)


-- | Returns the smaller of the two vertices connected
--   by an edge.
edgeX ∷ Edge → Vertex
edgeX = vertex . (`div` vmod) . unEdge


-- | Returns the larger of the two vertices connected
--   by an edge.
edgeY ∷ Edge → Vertex
edgeY = vertex . (`mod` vmod) . unEdge


-- | Returns the smaller of the two vertices connected
--   by an edge ans an Int.
edgeXInt ∷ Edge → Int
edgeXInt = unVertex . edgeX


-- | Returns the larger of the two vertices connected
--   by an edge ans an Int.
edgeYInt ∷ Edge → Int
edgeYInt = unVertex . edgeY


-- | Returns the two vertices connected by an edge in a list
edgeVertices ∷ Edge → [Vertex]
edgeVertices e = [edgeX e, edgeY e]


-- | True if one of the end points of edge e is vertex v
connects ∷ Edge → Vertex → Bool
connects e v = v == (edgeX e) || v == (edgeY e)


-- | True if the two edges share an end point and are not equal
edgesAdjacent ∷ Edge → Edge → Bool
edgesAdjacent e1 e2 = e1 /= e2 &&
                      (connects e2 (edgeX e1) || connects e2 (edgeY e1))


-- | A wrapper for sorted lists of edges without dublicates
newtype Edges = Edges { unEdges ∷ [Edge] }
  deriving (Show, Eq, Ord, NFData)


-- | Wraps a list of edges without sorting or checking
--   for dublicates
--
--   USE WITH CARE: Call this function only if you know that your list
--   of edges is sorted and without dublicates
unsafeEdges ∷ [Edge] → Edges
unsafeEdges = Edges

-- | The empty list of edges
emptyEdges ∷ Edges
emptyEdges = Edges []


edgesFromList ∷ [Edge] → Edges
edgesFromList = Edges . sortedUnique

completeEdges ∷ Int → Edges
completeEdges n = Edges [x<->y | x ← [0..n-1], y ← [x+1..n-1]]

chainEdges ∷ Int → Edges
chainEdges n = Edges [x<->(x+1) | x ← [0..n-2]]

edgesSize ∷ Edges → Int
edgesSize = length . unEdges

edgesNull ∷ Edges → Bool
edgesNull = null . unEdges

filterEdges ∷ (Edge → Bool) → Edges → Edges
filterEdges p = Edges . filter p . unEdges

maximumV ∷ Edges → Maybe Vertex
maximumV = maximumMay . fmap edgeY . unEdges

minimumV ∷ Edges → Maybe Vertex
minimumV = minimumMay . fmap edgeX . unEdges
