{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.Variable (htf_thisModulesTests) where

import Test.Framework

import Language.Typescript.Parser
import Language.Typescript.Parser.Types

success inp res = 
    case (parseString inp) of
      Left _ -> assertFailure "Error on parsing"
      Right [e] -> assertEqual e res

failed inp =
    case parseString inp of
      Left _ -> assertBool True
      Right _ -> assertFailure "Expected parsing error"

test_singleVarEmptyDefinition = 
    success "var x" $ VarDefinition $ [("x", Nothing)]

test_manyVarEmptyDefinition = 
    success "var x, y" $ VarDefinition $ [
                 ("x", Nothing),
                 ("y", Nothing)
                ]

test_dollarVarName = 
    success "var $" $ VarDefinition $ [("$", Nothing)]

test_underscoreName =
    success "var   _name$" $ VarDefinition $ [("_name$", Nothing)]

test_numberVariable =
    success "var x = 2" $ VarDefinition $ [("x", Just $ Number 2)]

test_multiInitial = 
    success "var x = 4, b" $ VarDefinition $ [
                 ("x", Just $ Number 4),
                 ("b", Nothing)
                ]

test_multiInitial2 = 
    success "var x, a = 4" $ VarDefinition $ [
                 ("x", Nothing),
                 ("a", Just $ Number 4)
                ]

test_expressionInInitial =
    failed "var x = "

test_endComma =
    failed "var x,"
