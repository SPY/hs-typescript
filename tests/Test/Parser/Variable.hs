{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.Variable (htf_thisModulesTests) where

import Test.Framework

import Language.Typescript.Parser

test_singleVarEmptyDefinition = 
    assertEqual (parse "var x") $ VarDefinition $ [("x", Nothing)]
