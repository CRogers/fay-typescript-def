module Main where

import Data.Functor.Identity
import Text.Parsec
import Language.TypeScript

main = putStrLn "hi"

parseTypeScript :: String -> Either ParseError [DeclarationElement]
parseTypeScript = runIdentity . runParserT declarationSourceFile () ""