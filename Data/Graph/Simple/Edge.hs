module Data.Graph.Simple.Edge (
  Edge, unEdge

, edgeX, edgeY, edgeXInt, edgeYInt
, edge, edgeMay, unsafeEdge, (<->)
, connects, transformEdge, edgeVertices
, edgesAdjacent, edgesSize
, edgesNull, filterEdges

,  Edges, unEdges, emptyEdges, edgesFromList, normalizeEdges
,  completeEdges, chainEdges
) where

import Control.Monad ((>>=))
import Data.Bool (Bool, otherwise, (||))
import Data.Eq (Eq, (==))
import Data.Function ((.), ($))
import Data.Functor (fmap)
import Data.Graph.Simple.Util (sortedUnique)
import Data.Graph.Simple.Vertex (Vertex, Vertices, unVertex, maxVertex, vertex, minVertex)
import Data.Int (Int)
import Data.List (zip, null, length, filter)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Ord (Ord, (<))
import GHC.Num ((+), (*), (-))
import GHC.Real (div, mod)
import Prelude (error)
import Text.Show (Show, show)
import qualified Data.Map as M

-- ** Edge ** --
--

newtype Edge = Edge { unEdge ∷ Int }
  deriving (Eq, Ord)

instance Show Edge where
  show e = show (edgeX e) <> " <-> " <> show (edgeY e)


-- | Creates an edge from two vertices
--   The two vertices must not be equal
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

(<->) ∷ Int → Int → Edge
x <-> y = edge (vertex x) (vertex y)


transformEdge ∷ (Vertex → Vertex) → Edge → Edge
transformEdge f e = edge (f $ edgeX e) (f $ edgeY e)


-- | Returns the smaller of the two vertices connected
--   by an edge.
edgeX ∷ Edge → Vertex
edgeX = vertex . (`div` vmod) . unEdge


-- | Returns the larger of the two vertices connected
--   by an edge.
edgeY ∷ Edge → Vertex
edgeY = vertex . (`mod` vmod) . unEdge

edgeXInt ∷ Edge → Int
edgeXInt = unVertex . edgeX

edgeYInt ∷ Edge → Int
edgeYInt = unVertex . edgeY


edgeVertices ∷ Edge → Vertices
edgeVertices e = [edgeX e, edgeY e]


-- | True if one of the end points of edge e is vertex v
connects ∷ Edge → Vertex → Bool
connects e v = v == (edgeX e) || v == (edgeY e)

-- | True if the two edges share an end point
edgesAdjacent ∷ Edge → Edge → Bool
edgesAdjacent e1 e2 = connects e2 (edgeX e1) || connects e2 (edgeY e1)



newtype Edges = Edges { unEdges ∷ [Edge] }
  deriving (Show, Eq, Ord)

emptyEdges ∷ Edges
emptyEdges = Edges []

edgesFromList ∷ [Edge] → Edges
edgesFromList = Edges . sortedUnique

completeEdges ∷ Int → Edges
completeEdges n = Edges $ [x<->y | y ← [1..n-1], x ← [0..y-1]]

chainEdges ∷ Int → Edges
chainEdges n = Edges $ [x<->(x+1) | x ← [0..n-2]]


-- | Takes a list of arbitrary edges and transforms them
--   to link vertices from 1 to n where n is the total
--   number of distinct vertices found in the list.
normalizeEdges ∷ Edges → Edges
normalizeEdges (Edges es) = Edges $ fmap (transformEdge (map M.!)) es
  where map = M.fromList $ zip vs [minVertex..]
        vs  = sortedUnique es >>= edgeVertices

edgesSize ∷ Edges → Int
edgesSize = length . unEdges

edgesNull ∷ Edges → Bool
edgesNull = null . unEdges

filterEdges ∷ (Edge → Bool) → Edges → Edges
filterEdges p = Edges . filter p . unEdges
