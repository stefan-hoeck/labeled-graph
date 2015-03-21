{-# OPTIONS_GHC -fno-warn-orphans #-}

module GraphQC (
  smallVertex
, smallEdge
, smallEdges
, SmallVertex(..)
, SmallEdge(..)
, SmallEdges(..)
) where

import Data.Graph.Simple.Edge
import Data.Graph.Simple.Vertex
import Test.Framework

unequal ∷ Eq a ⇒ (a,a) → Bool
unequal = uncurry (/=)

{- Vertex Generators -}

smallVertex ∷ Gen Vertex
smallVertex = fmap getSmallVertex arbitrary

instance Arbitrary Vertex where
  arbitrary = fmap unsafeVertex $ choose (minVAsInt, maxVAsInt)

newtype SmallVertex = SmallVertex { getSmallVertex ∷ Vertex }
  deriving (Eq, Show, Ord, Bounded)

instance Arbitrary  SmallVertex where
  arbitrary = fmap (SmallVertex . unsafeVertex) $ choose (minVAsInt, 5)


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
