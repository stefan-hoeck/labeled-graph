{-# OPTIONS_GHC -F -pgmF htfpp #-}
module VertexTests (htf_thisModulesTests) where

import Data.Maybe (isNothing, isJust)
import Data.Graph.Simple.Vertex
import Test.Framework

prop_isValidVertex_negativeFalse ∷ Int → Bool
prop_isValidVertex_negativeFalse x | x < 0     = isNothing $ vertexMay x
                                   | otherwise = True

prop_isValidVertex_maxIntFalse ∷ () → Bool
prop_isValidVertex_maxIntFalse _ = isNothing $ vertexMay maxBound

prop_isValidVertex_smallOK ∷ Int → Bool
prop_isValidVertex_smallOK x | x >= 0 && x <= maxVAsInt = isJust $ vertexMay x
                             | otherwise                = True

prop_unVertex ∷ Int → Bool
prop_unVertex x = maybe True ((x==) . unVertex) $ vertexMay x
