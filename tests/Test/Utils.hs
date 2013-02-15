{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Utils (parseString, makeSuccess, makeFailed) where

import Test.Framework

import Text.Parsec

parseString parser inp = runParser parser () "" inp

makeSuccess parser constr inp res = 
    case parseString parser inp of
      Left _ -> assertFailure "Error on parsing"
      Right e -> assertEqual (constr $ res) e

makeFailed parser inp =
    case parseString parser inp of
      Left _ -> assertBool True
      Right _ -> assertFailure "Expected parsing error"
