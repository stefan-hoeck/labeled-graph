{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GraphTests (htf_thisModulesTests) where

import GraphQC
import Data.Graph.Simple.Graph
import Data.List (sort)
import Safe (headMay)
import Test.Framework

import Prelude hiding (null)

prop_graphDist ∷ Graph → Property
prop_graphDist g = classify (isNull g)      "Null graph" $
                   classify (isEmpty g)     "Empty graph" $
                   classify (isConnected g) "Connected graph" $
                   classify (isComplete g)  "Complete graph" $
                   classify (size g > 20)   "Decent size" $
                   True

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

prop_complete ∷ Property
prop_complete = forAll completeGraph valid
    where valid g = all (uncurry $ adjacent g) $ vertexPairs g

prop_complete_isComplete ∷ Property
prop_complete_isComplete = forAll completeGraph isComplete

prop_chain ∷ Property
prop_chain = forAll chainGraph valid
    where valid g = all (\v → adjacent g v (v+1)) $ init (vertices g)

prop_complement_order ∷ Graph → Bool
prop_complement_order g = let g' = complement g
                          in order g == order g'

prop_complement ∷ Graph → Bool
prop_complement g = let g' = complement g
                        inGorG' (x,y) = adjacent g x y || adjacent g' x y
                    in  all inGorG' $ vertexPairs g

prop_vertices_size ∷ Graph → Bool
prop_vertices_size g = (length $ vertices g) == order g

prop_vertices_sorted ∷ Graph → Bool
prop_vertices_sorted g = let vs = vertices g
                         in  vs == sort vs

prop_vertices_head ∷ Graph → Bool
prop_vertices_head g = let ho = headMay $ vertices g
                       in  case order g of
                             0 → ho == Nothing
                             _ → ho == Just minBound

prop_minmaxDegree ∷ Graph → Bool
prop_minmaxDegree g = minDegree g <= maxDegree g

prop_neighbors_are_adjacent ∷ Graph → Bool
prop_neighbors_are_adjacent g = let ok v = all (adjacent g v) $ neighbors g v
                                in  all ok $ vertices g

prop_sum_of_degrees ∷ Graph → Bool
prop_sum_of_degrees g = (sum $ degrees g) == (2 * size g)

prop_connected_subgraph_orders ∷ Graph → Bool
prop_connected_subgraph_orders g = let gs = fmap fst $ connectedSubgraphs g
                                       o' = sum $ fmap order gs
                                   in  o' == order g

prop_connected_subgraph_sizes ∷ Graph → Bool
prop_connected_subgraph_sizes g = let gs = fmap fst $ connectedSubgraphs g
                                      s' = sum $ fmap size gs
                                  in  s' == size g

prop_edgeIn ∷ Graph → Bool
prop_edgeIn g = all (edgeIn g) $ edgeList g

prop_edgeIn_neighbors ∷ Graph → Bool
prop_edgeIn_neighbors g = let ok v = all (edgeIn g . edge v) $ neighbors g v
                          in  all ok $ vertices g

prop_edgeIn_edgesAt ∷ Graph → Bool
prop_edgeIn_edgesAt g = let ok v = all (edgeIn g) $ edgesAt g v
                        in  all ok $ vertices g

prop_edgesAt_connect ∷ Graph → Bool
prop_edgesAt_connect g = let ok v = all (flip connects v) $ edgesAt g v
                         in  all ok $ vertices g

vertexPairs ∷ Graph → [(Vertex, Vertex)]
vertexPairs g = let vs = vertices g
                in  [(x,y) | x ← vs, y ← vs, x /= y]
