{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.String (htf_thisModulesTests) where

import Test.Framework

import Language.Typescript.Parser
import Language.Typescript.Parser.Types

success inp res = 
    case (parseString inp) of
      Left _ -> assertFailure "Error on parsing"
      Right [e] -> assertEqual e $ SString res

failed inp =
    case parseString inp of
      Left _ -> assertBool True
      Right _ -> assertFailure "Expected parsing error"

test_emptyStringSingleQuote =
    success "''" ""

test_emptyStringDoubleQuote = 
    success "\"\"" ""

test_singleQuoutes = 
    success "'abc'" "abc"

test_unicodeString =
    success "'оло'" "оло"

test_nestedQuotes = 
    success "'abc\"dfg'" "abc\"dfg"

test_escapedQuote =
    success "'abc\\'dfg'" "abc'dfg"
