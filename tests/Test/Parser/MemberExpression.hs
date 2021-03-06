{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.MemberExpression (htf_thisModulesTests) where

import Test.Framework
import Test.Utils

import Language.Typescript.Types
import Language.Typescript.Parser.Expression.Member

success = makeSuccess memberExpression MemberExpression

failed = makeFailed memberExpression

{- helpers -}
simple = SimpleMember . PrimaryExpression
simpleX = simple x
xDotY = Dot simpleX y

{- tests -}
test_singleDotMember =
    success "x.y" $ Dot simpleX y

test_doubleDotMembera = 
    success "x.y.z" $ Dot (Dot simpleX y) z

test_bracketsMember =
    success "x['y']" $ Brackets simpleX yString

test_mixedMemberExpression =
    success "x['y'].z" $ Dot (Brackets simpleX yString) z

test_nestedMemberExpression =
    success "x[ x . y]" $ Brackets simpleX $ MemberExpression $ Dot simpleX y

test_thisMemberExpression =
    success "this.x" $ Dot (simple This) x

test_parenthesesMemberExpression =
    success "(this).x" $ Dot (SimpleMember $ PrimaryExpression $ Parentheses $ PrimaryExpression This) x
