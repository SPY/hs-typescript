module Language.Typescript.Parser.Types where

import Text.Parsec (Parsec)

type TSParser a = Parsec String () a
