{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.Variable (htf_thisModulesTests) where

import Test.Framework

import Language.Typescript.Parser
import Language.Typescript.Parser.Types

success inp res = 
    case (parseString inp) of
      Left _ -> assertFailure "Error on parsing"
      Right [e] -> assertEqual e $ SVarDefinition res

failed inp =
    case parseString inp of
      Left _ -> assertBool True
      Right _ -> assertFailure "Expected parsing error"

test_singleVarEmptyDefinition = 
    success "var x" [("x", Nothing)]

test_manyVarEmptyDefinition = 
    success "var x, y" [
                 ("x", Nothing),
                 ("y", Nothing)
                ]

test_dollarVarName = 
    success "var $" [("$", Nothing)]

test_underscoreName =
    success "var   _name$" [("_name$", Nothing)]

test_numberVariable =
    success "var x = 2" [("x", Just $ SNumber 2)]

test_multiInitial = 
    success "var x = 4, b" [
                 ("x", Just $ SNumber 4),
                 ("b", Nothing)
                ]

test_multiInitial2 = 
    success "var x, a = 4" [
                 ("x", Nothing),
                 ("a", Just $ SNumber 4)
                ]

test_initByString = 
    success "var s = 'string'" [("s", Just $ SString "string")]

test_expressionInInitial =
    failed "var x = "

test_endComma =
    failed "var x,"
