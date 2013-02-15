{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.Number (htf_thisModulesTests) where

import Test.Framework
import Test.Utils

import Language.Typescript.Types
import Language.Typescript.Parser.Literal

success = makeSuccess numericLiteral Numeric

failed = makeFailed numericLiteral

test_singleDigit = 
    success "3" 3
            
test_complexNumber =
    success "42" 42

test_floatNumber =
    success "2.3" 2.3

test_singleDot = 
    failed "."

test_zero = 
    success "0." 0

test_floatZero =
    success ".0" 0

test_notBracedDot =
    success ".3" 0.3

test_notBracedDot1 =
    success "3." 3

test_scinificNotation =
    success ".5e1" 5

test_scinificNegate =
    success "3.5e-1" $ 3.5*0.1

test_positiveScinificNotation = 
    success "1.0e+3" 1000

test_integerScinificNotation =
    success "1e-3" 0.001

