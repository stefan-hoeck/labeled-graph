{-# OPTIONS_GHC -F -pgmF htfpp #-}
module VertexTests (htf_thisModulesTests) where

import Data.Maybe (isNothing, isJust)
import Data.Graph.Simple.Vertex
import Test.Framework

prop_isValidVertex_negativeFalse ∷ Int → Property
prop_isValidVertex_negativeFalse x = x < 0  ==> isNothing $ vertexMay x

prop_isValidVertex_maxIntFalse ∷ () → Bool
prop_isValidVertex_maxIntFalse _ = isNothing $ vertexMay maxBound

prop_isValidVertex_smallOK ∷ Int → Property
prop_isValidVertex_smallOK v = isValid v ==> isJust $ vertexMay v

prop_unVertex ∷ Int → Property
prop_unVertex x = isValid x ==> Just x == fmap unVertex (vertexMay x)

prop_vertex_withinBounds ∷ Int → Bool
prop_vertex_withinBounds = isValid . unVertex . vertex

prop_vertex_valid_id ∷ Int → Property
prop_vertex_valid_id x = isValid x ==>  x == (unVertex . vertex) x

prop_vertex_mod ∷ Int → Bool
prop_vertex_mod x = x `mod` maxVAsInt == (unVertex . vertex) x


isValid ∷ Int → Bool
isValid x = x >= 0 && x <= maxVAsInt

instance Arbitrary Vertex where
  arbitrary = fmap unsafeVertex $ choose (minVAsInt, maxVAsInt)
