{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.PrimaryExpression (htf_thisModulesTests) where

import Test.Framework
import Test.Utils

import Language.Typescript.Types
import Language.Typescript.Parser.Expression.Primary

success = makeSuccess primaryExpression PrimaryExpression

failed = makeFailed primaryExpression

test_thisExpression =
    success "this" $ This

test_identifier =
    success "$" $ Identifier "$"

test_nullLiteral =
    success "null" $ PrimaryLiteral $ Null

test_falseLiteral =
    success "false" $ PrimaryLiteral $ Boolean $ False

test_parenthesesLiteral =
    success "( null )" $ Parentheses $
            MemberExpression $ SimpleMember $ 
            PrimaryExpression $ PrimaryLiteral $ Null

test_parenthesesIdentifier =
    success "(_ident)" $ Parentheses $ MemberExpression $ 
            SimpleMember $ PrimaryExpression $ Identifier "_ident"
