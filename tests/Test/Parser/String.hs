{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.String (htf_thisModulesTests) where

import Test.Framework
import Test.Utils

import Language.Typescript.Types
import Language.Typescript.Parser.Literal

success = makeSuccess stringLiteral StringLiteral

failed = makeFailed stringLiteral

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

test_unicodeSeq = 
    success "'\\u0030'" "0"

test_hexSeq =
    success "'\\x30'" "0"

test_mixSeq = 
    success "\"\\x30\\u00300f\"" "000f"

test_slashN =
    success "'string\\\nanother'" "string\nanother"

test_escapedChar =
    success "'text\\ttext'" "text\ttext"

