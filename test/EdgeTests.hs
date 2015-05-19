{-# OPTIONS_GHC -F -pgmF htfpp #-}

module EdgeTests (htf_thisModulesTests) where

import Data.List (sort)
import Data.Maybe (isNothing)
import Data.Graph.Simple.Edge
import Data.Graph.Simple.Vertex
import GraphQC
import Safe (minimumMay, maximumMay)
import Test.Framework

import qualified Data.Set as S

{- First we check that all Edge constructors give the same
   internal representation. Afterwards we only need to
   test a single constructor -}
prop_edgeMay ∷ Vertex → Vertex → Bool
prop_edgeMay v1 v2 = isNothing (edgeMay v1 v2) == (v1 == v2)

prop_edgeMay_likeUnsafeEdge ∷ Vertex → Vertex → Property
prop_edgeMay_likeUnsafeEdge v1 v2 = v1 /= v2 ==> edgeMay v1 v2 == Just (unsafeEdge v1' v2')
    where v1' = min v1 v2
          v2' = max v1 v2

prop_edge_likeUnsafeEdge ∷ Vertex → Vertex → Property
prop_edge_likeUnsafeEdge v1 v2 = v1 /= v2 ==> edge v1 v2 == unsafeEdge v1' v2'
    where v1' = min v1 v2
          v2' = max v1 v2

prop_edgeFromInt ∷ Vertex → Vertex → Property
prop_edgeFromInt v1 v2 = v1 /= v2 ==> edge v1 v2 ==
                                      (unVertex v1) <-> (unVertex v2)


{- Extracting vertices from edges -}

prop_edgeX ∷ Vertex → Vertex → Property
prop_edgeX v1 v2 = v1 /= v2 ==> edgeX (edge v1 v2) == min v1 v2

prop_edgeY ∷ Vertex → Vertex → Property
prop_edgeY v1 v2 = v1 /= v2 ==> edgeY (edge v1 v2) == max v1 v2

prop_edgeXInt ∷ Vertex → Vertex → Property
prop_edgeXInt v1 v2 = v1 /= v2 ==> edgeXInt (edge v1 v2) ==
                                   unVertex (min v1 v2)

prop_edgeYInt ∷ Vertex → Vertex → Property
prop_edgeYInt v1 v2 = v1 /= v2 ==> edgeYInt (edge v1 v2) ==
                                   unVertex (max v1 v2)

prop_transformEdge ∷ SmallEdge → Bool
prop_transformEdge (SmallEdge e) = let e'  = transformEdge (+1) e
                                       vs  = edgeVertices e
                                       vs' = edgeVertices e'
                                   in fmap (+1) vs == vs'

prop_edgeVertices ∷ Vertex → Vertex → Property
prop_edgeVertices v1 v2 = v1 /= v2 ==> sort [v1,v2] ==
                                       edgeVertices (edge v1 v2)


{- Edge Connectivity -}

prop_connects ∷ Vertex → Vertex → Property
prop_connects v1 v2 = let e = edge v1 v2
                      in  v1 /= v2 ==> e `connects` v1 && e `connects` v2

prop_connects_other ∷ Edge → Vertex → Bool
prop_connects_other e v = e `connects` v == (v `elem` edgeVertices e)

prop_edgeAdjacent_self_true ∷ Edge → Bool
prop_edgeAdjacent_self_true e = edgesAdjacent e e

prop_edgeAdjacent ∷ Edge → Edge → Bool
prop_edgeAdjacent e1 e2 = let vs1 = edgeVertices e1
                              vs2 = edgeVertices e2
                          in  edgesAdjacent e1 e2 == any (`elem` vs1) vs2


{- Edges -}

prop_edges_valid ∷ Edges → Bool
prop_edges_valid = validEdges

prop_edgesFromList_valid ∷ [Edge] → Bool
prop_edgesFromList_valid = validEdges . edgesFromList

prop_completeEdges_valid ∷ Small Int → Bool
prop_completeEdges_valid = validEdges . completeEdges . abs . getSmall

prop_completeEdges_size ∷ Small Int → Bool
prop_completeEdges_size (Small n) = let n' = abs n
                                        len = edgesSize $ completeEdges n'
                                    in  len == (n' * (n'-1)) `div` 2

prop_completeEdges_max ∷ Small Int → Property
prop_completeEdges_max (Small n) = let n' = abs n
                                       es = unEdges $ completeEdges n'
                                       mx = maximum es
                                   in  n' > 1 ==> mx == ((n'-2) <-> (n'-1))

prop_chainEdges_valid ∷ Small Int → Bool
prop_chainEdges_valid = validEdges . chainEdges . abs . getSmall

prop_chainEdges_size ∷ Small Int → Property
prop_chainEdges_size (Small n) = let n' = abs n
                                     len = edgesSize $ chainEdges n'
                                 in  n' > 1 ==> len == n' - 1

prop_chainEdges_max ∷ Small Int → Property
prop_chainEdges_max (Small n) = let n' = abs n
                                    es = unEdges $ chainEdges n'
                                    mx = maximum es
                                in  n' > 1 ==> mx == ((n'-2) <-> (n'-1))

prop_edgesSize ∷ Edges → Bool
prop_edgesSize es = edgesSize es == length (unEdges es)

prop_edgesNull ∷ Edges → Bool
prop_edgesNull es = edgesNull es == null (unEdges es)

prop_filterEdges ∷ Edges → Bool
prop_filterEdges es = let p   = flip connects 1
                          es' = unEdges $ filterEdges p es
                      in filter p (unEdges es) == es'

prop_minimumV ∷ Edges → Bool
prop_minimumV es = let m = minimumV es
                       ok = (m <=) . minimumMay . edgeVertices
                   in  all ok $ unEdges es

prop_maximumV ∷ Edges → Bool
prop_maximumV es = let m = maximumV es
                       ok = (m >=) . maximumMay . edgeVertices
                   in  all ok $ unEdges es

validEdges ∷ Edges → Bool
validEdges es = let es' = unEdges es
                in  es' == S.toList (S.fromList es')
