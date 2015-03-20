{-# OPTIONS_GHC -F -pgmF htfpp #-}
module EdgeTests (htf_thisModulesTests) where

import Data.Maybe (isNothing)
import Data.Graph.Simple.Edge
import Data.Graph.Simple.Vertex
import Test.Framework
import VertexTests ()

prop_edgeMay ∷ Vertex → Vertex → Bool
prop_edgeMay v1 v2 = isNothing (edgeMay v1 v2) == (v1 == v2)

prop_edgeMay_callsUnsafeEdge ∷ Vertex → Vertex → Property
prop_edgeMay_callsUnsafeEdge v1 v2 = v1 /= v2 ==> edgeMay v1 v2 == Just (unsafeEdge v1' v2')
    where v1' = min v1 v2
          v2' = max v1 v2

prop_edge_callsUnsafeEdge ∷ Vertex → Vertex → Property
prop_edge_callsUnsafeEdge v1 v2 = v1 /= v2 ==> edge v1 v2 == unsafeEdge v1' v2'
    where v1' = min v1 v2
          v2' = max v1 v2

