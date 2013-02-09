{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.Number (htf_thisModulesTests) where

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

test_singleDigit = 
    success "3" $ Number 3
            
test_complexNumber =
    success "42" $ Number 42

test_floatNumber =
    success "2.3" $ Number 2.3

test_singleDot = 
    failed "."

test_notBracedDot =
    success ".3" $ Number 0.3

test_notBracedDot1 =
    success "3." $ Number 3
