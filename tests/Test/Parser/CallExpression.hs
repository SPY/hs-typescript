{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.CallExpression (htf_thisModulesTests) where

import Test.Framework
import Test.Utils

import Language.Typescript.Types
import Language.Typescript.Parser.Expression.Call

successWithX = makeSuccess callExpression $ CallExpression $ PrimaryExpression x
successWithXZ = makeSuccess callExpression $ CallExpression $ xz
                
failed = makeFailed callExpression

test_emptyCallExpression = 
    successWithX "x ()" []

test_callWithOneArgument =
    successWithX "x('y', x.z)" $ [yString, xz]
