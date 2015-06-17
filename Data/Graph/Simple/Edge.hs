{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module        : Data.Graph.Simple.Edge
Description   : Undirected edges in simple graphs
Copyright     : Stefan Höck
Maintainer    : Stefan Höck
Stability     : experimental

Provides a memory-efficient representation for undirected edges.
Internally, an edge is stored as an Int from which the two
Vertices connected by the edge can be extracted using modular
arithmetic.

In addition, a wrapper for lists of edges to be used in graphs
is provided. This guarantees that lists of edges are always
sorted and hold no dublicates. Combining lists of edges
can thus be done in linear time.
-}
module Data.Graph.Simple.Edge (
-- * Edge

-- ** Class
  Edge, unEdge

-- ** Construction
, edge, edgeMay, unsafeEdge, (<->)

-- ** Vertices from edges
, edgeX, edgeY, edgeXInt, edgeYInt
, edgeVertices

-- ** Connectivity of edges
, connects, edgesAdjacent


-- * Edges

-- ** Class
,  Edges, unEdges

-- ** Construction
,  unsafeEdges, emptyEdges, edgesFromList
,  completeEdges, chainEdges, edgesFromVertices

-- ** Properties of edge lists
,  edgesSize, edgesNull,  maximumV, minimumV

-- ** Filtering edges
,  filterEdges

-- ** Set operations
,  edgesUnion, edgesDifference, edgesSymmetricDifference
) where

import Control.DeepSeq (NFData)
import Data.Graph.Simple.Util
import Data.Graph.Simple.Vertex (Vertex, unVertex, maxVertex, vertex)
import Data.Monoid ((<>))
import Safe (maximumMay, minimumMay)

-- * Edge

-- | An edge in a simple graph
--
--   Internally, the edge is stored as an int using
--   modular arithmetik to extraxt the two vertices from
--   it. This leads to a very compact representation.
newtype Edge = Edge { unEdge ∷ Int }
  deriving (Eq, Ord, NFData)

instance Show Edge where
  show e = show (edgeXInt e) <> " <-> " <> show (edgeYInt e)


-- ** Construction

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


vmod ∷ Int
vmod = 1 + (unVertex maxVertex)


-- ** Vertices from edges

-- | Returns the smaller of the two vertices connected
--   by an edge.
edgeX ∷ Edge → Vertex
edgeX = vertex . (`div` vmod) . unEdge


-- | Returns the larger of the two vertices connected
--   by an edge.
edgeY ∷ Edge → Vertex
edgeY = vertex . (`mod` vmod) . unEdge


-- | Returns the smaller of the two vertices connected
--   by an edge as an Int.
edgeXInt ∷ Edge → Int
edgeXInt = unVertex . edgeX


-- | Returns the larger of the two vertices connected
--   by an edge as an Int.
edgeYInt ∷ Edge → Int
edgeYInt = unVertex . edgeY


-- | Returns the two vertices connected by an edge in a list
edgeVertices ∷ Edge → [Vertex]
edgeVertices e = [edgeX e, edgeY e]




-- ** Connectivity of edges

-- | True if one of the end points of edge e is vertex v
connects ∷ Edge → Vertex → Bool
connects e v = v == (edgeX e) || v == (edgeY e)


-- | True if the two edges share an end point and are not equal
edgesAdjacent ∷ Edge → Edge → Bool
edgesAdjacent e1 e2 = e1 /= e2 &&
                      (connects e2 (edgeX e1) || connects e2 (edgeY e1))





-- * Edges

-- | A wrapper for sorted lists of edges without dublicates
newtype Edges = Edges { unEdges ∷ [Edge] }
  deriving (Show, Eq, Ord, NFData)


-- ** Construction

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


-- | Takes a list of edges, which is then being sorted
--   and dublicates are removed.
edgesFromList ∷ [Edge] → Edges
edgesFromList = Edges . sortedUnique

-- | Takes a list of vertices and creates edges between
--   successors in this list.
edgesFromVertices ∷ [Vertex] → Edges
edgesFromVertices []       = emptyEdges 
edgesFromVertices vs@(_:t) = edgesFromList $ zipWith edge vs t


-- | Returns the edges of a complete graph of the given order
completeEdges ∷ Int → Edges
completeEdges n = Edges [x<->y | x ← [0..n-1], y ← [x+1..n-1]]


-- | Returns the edges of a chain graph of the given order
chainEdges ∷ Int → Edges
chainEdges n = Edges [x<->(x+1) | x ← [0..n-2]]



-- ** Properties of edge lists

-- | Number of edges
edgesSize ∷ Edges → Int
edgesSize = length . unEdges


-- | True if the 'Edges' are empty
edgesNull ∷ Edges → Bool
edgesNull = null . unEdges


-- | The maximum vertex in a list of edges.
maximumV ∷ Edges → Maybe Vertex
maximumV = maximumMay . fmap edgeY . unEdges


-- | The minimum vertex in a list of edges.
minimumV ∷ Edges → Maybe Vertex
minimumV = minimumMay . fmap edgeX . unEdges




-- ** Filtering edges

filterEdges ∷ (Edge → Bool) → Edges → Edges
filterEdges p = Edges . filter p . unEdges



-- ** Set operations

-- | Returns the union of two sets of edges
edgesUnion ∷ Edges → Edges → Edges
Edges a `edgesUnion` Edges b = Edges $ sortedUnion a b


-- | Removes the edges in the second set from those in
--   the first set
edgesDifference ∷ Edges → Edges → Edges
Edges a `edgesDifference` Edges b = Edges $ sortedDiff a b


-- | Returns the union of two sets of edges from which
--   the intersection (common edges) of the two sets is
--   removed.
edgesSymmetricDifference ∷ Edges → Edges → Edges
Edges a `edgesSymmetricDifference` Edges b = Edges $ sortedSymmDiff a b
