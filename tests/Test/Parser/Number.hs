{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.Number (htf_thisModulesTests) where

import Test.Framework

import Language.Typescript.Parser
import Language.Typescript.Parser.Types

success inp res = 
    case (parseString inp) of
      Left _ -> assertFailure "Error on parsing"
      Right [e] -> assertEqual e $ SNumber res

failed inp =
    case parseString inp of
      Left _ -> assertBool True
      Right _ -> assertFailure "Expected parsing error"

test_singleDigit = 
    success "3" 3
            
test_complexNumber =
    success "42" 42

test_floatNumber =
    success "2.3" 2.3

test_singleDot = 
    failed "."

test_notBracedDot =
    success ".3" 0.3

test_notBracedDot1 =
    success "3." 3

test_scinificNotation =
    success ".5e1" 5

test_scinificNegate =
    success "3.5e-1" 0.35

test_positiveScinificNotation = 
    success "1.0e+3" 1000

test_integerScinificNotation =
    success "1e-3" 0.001

