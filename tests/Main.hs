{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

-- The main test program.

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Test.Parser.Variable
import {-@ HTF_TESTS @-} Test.Parser.Number
import {-@ HTF_TESTS @-} Test.Parser.String
import {-@ HTF_TESTS @-} Test.Parser.Function
import {-@ HTF_TESTS @-} Test.Parser.Object

main = htfMain htf_importedTests
