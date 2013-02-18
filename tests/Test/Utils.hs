{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Utils where

import Test.Framework

import Text.Parsec

import Language.Typescript.Types

parseString parser inp = runParser parser () "" inp

makeSuccess parser constr inp res = 
    case parseString parser inp of
      Left _ -> assertFailure "Error on parsing"
      Right e -> assertEqual (constr $ res) e

makeFailed parser inp =
    case parseString parser inp of
      Left _ -> assertBool True
      Right _ -> assertFailure "Expected parsing error"

x = Identifier "x"
y = Identifier "y"
z = Identifier "z"
yString = PrimaryExpression $ PrimaryLiteral $ StringLiteral "y"

xz = MemberExpression $ Dot (SimpleMember $ PrimaryExpression x) $ z
