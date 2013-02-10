{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.Object (htf_thisModulesTests) where

import Test.Framework

import Language.Typescript.Parser
import Language.Typescript.Parser.Types

success inp res = 
    case (parseString inp) of
      Left err -> assertFailure $ "Error on parsing:" ++ show err
      Right [e] -> assertEqual e $ SObject res

failed inp =
    case parseString inp of
      Left _ -> assertBool True
      Right _ -> assertFailure "Expected parsing error"

test_emptyObjectLiteral =
    success "{}" []

test_singleFieldObject =
    success "{ x: 3 }" [("x", SNumber 3)]

test_singleFieldWithTrailingComma = 
    success "{ x : 42, }" [("x", SNumber 42)]

test_singleFieldObjectWithStringKey =
    success "{ 'x': 3 }" [("x", SNumber 3)]

test_miltiFiledsObject =
    success "{ 'x': 3, y: 4 }" [("x", SNumber 3), ("y", SNumber 4)]
