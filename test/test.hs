{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main (main, htf_Main_thisModulesTests) where

import {-@ HTF_TESTS @-} EdgeTests
import {-@ HTF_TESTS @-} VertexTests
import {-@ HTF_TESTS @-} UtilTests
import Test.Framework

main ∷ IO ()
main = htfMain htf_importedTests
