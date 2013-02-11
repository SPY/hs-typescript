{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parser.Object (htf_thisModulesTests) where

import Test.Framework

import Language.Typescript.Parser
import Language.Typescript.Parser.Token

success inp res = 
    case (parseString inp) of
      Left err -> assertFailure $ "Error on parsing:" ++ show err
      Right [e] -> assertEqual e $ SObject res

failed inp =
    case parseString inp of
      Left _ -> assertBool True
      Right _ -> assertFailure "Expected parsing error"
