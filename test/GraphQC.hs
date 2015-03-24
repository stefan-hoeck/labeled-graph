{-# OPTIONS_GHC -fno-warn-orphans #-}

module GraphQC (
  emptyGraph
, maxSmallVertex
, smallVertex
, smallEdge
, smallEdges
, EmptyGraph(..)
, SmallVertex(..)
, SmallEdge(..)
, SmallEdges(..)
) where

import Data.Graph.Simple.Graph
import Test.Framework

unequal ∷ Eq a ⇒ (a,a) → Bool
unequal = uncurry (/=)

{- Vertex Generators -}

maxSmallVertex ∷ Int
maxSmallVertex = 7

smallVertex ∷ Gen Vertex
smallVertex = fmap getSmallVertex arbitrary

instance Arbitrary Vertex where
  arbitrary = fmap unsafeVertex $ choose (minVAsInt, maxVAsInt)

newtype SmallVertex = SmallVertex { getSmallVertex ∷ Vertex }
  deriving (Eq, Show, Ord, Bounded)

instance Arbitrary  SmallVertex where
  arbitrary = fmap (SmallVertex . unsafeVertex) $
              choose (minVAsInt, maxSmallVertex)


{- Edge Generators -}

smallEdge ∷ Gen Edge
smallEdge = fmap getSmallEdge arbitrary

smallEdges ∷ Gen Edges
smallEdges = fmap getSmallEdges arbitrary

instance Arbitrary Edge where
  arbitrary = fmap (uncurry edge) $ arbitrary `suchThat` unequal

instance Arbitrary Edges where
  arbitrary = fmap edgesFromList $ listOf arbitrary


newtype SmallEdge = SmallEdge { getSmallEdge ∷ Edge }
  deriving (Eq, Show, Ord)

instance Arbitrary SmallEdge where
  arbitrary = fmap toSmallEdge $ arbitrary `suchThat` unequal
    where toSmallEdge (a,b) = SmallEdge $
                              edge (getSmallVertex a)(getSmallVertex b)


newtype SmallEdges = SmallEdges { getSmallEdges ∷ Edges }
  deriving (Eq, Show, Ord)

instance Arbitrary SmallEdges where
  arbitrary = fmap (SmallEdges . edgesFromList) $ listOf smallEdge


{- Graph Generators -}

emptyGraph ∷ Gen Graph
emptyGraph = fmap getEmptyGraph arbitrary

instance Arbitrary Graph where
  arbitrary = fmap (fromEdges' . getSmallEdges) arbitrary


newtype EmptyGraph = EG { getEmptyGraph ∷ Graph }

instance Arbitrary EmptyGraph where
  arbitrary = fmap (EG . empty . getSmall) arbitrary
