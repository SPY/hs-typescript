{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.NewExpression (htf_thisModulesTests) where

import Test.Framework
import Test.Utils

import Language.Typescript.Types
import Language.Typescript.Parser.Expression.New

successWithX = makeSuccess newExpression $ NewExpression $ PrimaryExpression x
successWithXZ = makeSuccess newExpression $ NewExpression $ 
                MemberExpression $ Dot (SimpleMember $ PrimaryExpression x) $ z

failed = makeFailed newExpression

test_simpleNewExpression = 
    successWithX "new x()" []

test_simpleNewExpressionWithArgs =
    successWithX "new x('y')" [yString]

test_newExpressionWithoutArgs =
    successWithX "new x" []

test_idnentifierNewExpression =
    successWithXZ "new x.z('y', 'y')" [yString, yString]

test_newExprWithSpaces =
    successWithX "new x ( 'y' )" [yString]
