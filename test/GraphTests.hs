{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GraphTests (htf_thisModulesTests) where

import GraphQC
import Data.Graph.Simple.Graph
import Test.Framework

import Prelude hiding (null)

prop_null_isNull ∷ () → Bool
prop_null_isNull _ = isNull null

prop_empty_isEmpty ∷ Vertex → Bool
prop_empty_isEmpty = isEmpty . empty . unVertex

prop_empty_order ∷ SmallVertex → Bool
prop_empty_order sv = let o = unVertex $ getSmallVertex sv
                      in  o == order (empty o)

prop_fromEdges_edges ∷ SmallEdges → Bool
prop_fromEdges_edges (SmallEdges es) =
  es == edges (fromEdges (maxSmallVertex+1) es)

prop_fromEdges'_edges ∷ SmallEdges → Bool
prop_fromEdges'_edges (SmallEdges es) = es == edges (fromEdges' es)
