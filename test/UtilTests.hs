{-# OPTIONS_GHC -F -pgmF htfpp #-}
module UtilTests (htf_thisModulesTests) where

import Data.List (sort)
import Data.Graph.Simple.Util
import Test.Framework

prop_sortedUnique_isSorted ∷ [Int] → Bool
prop_sortedUnique_isSorted xs = let xs' = sortedUnique xs
                                in  sort xs' == xs'

prop_sortedUnique_isUnique ∷ [Int] → Bool
prop_sortedUnique_isUnique xs = let xs' = sortedUnique xs
                                    isUnique x = filter (x==) xs' == [x]
                                in  all isUnique xs
