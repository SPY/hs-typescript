{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.Function (htf_thisModulesTests) where

import Test.Framework

import Language.Typescript.Parser
import Language.Typescript.Parser.Types

success inp name body args = 
    case (parseString inp) of
      Left err -> assertFailure $ show err--"Error on parsing"
      Right [e] -> assertEqual e $ SFunDefinition name body args

failed inp =
    case parseString inp of
      Left _ -> assertBool True
      Right _ -> assertFailure "Expected parsing error"

test_parseEmptyFunction = 
    success "function f() {}" (Just "f") [] []

test_anonymousFunction =
    success "function(){}" Nothing [] []

test_incorrectDeclaration = 
    failed "functionf() {}"

test_functionWithArgs =
    success "function f(x) { }" (Just "f") [] ["x"]

test_severalArgsFunction =
    success "function(x,y,$){}" Nothing [] ["x", "y", "$"]

test_notEmptyFunction =
    success "function() { var x }" Nothing [SVarDefinition [("x", Nothing)]] []
