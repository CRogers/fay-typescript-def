module Main where

import Control.Monad
import Data.Functor.Identity
import Text.Parsec
import Language.TypeScript

main = putStrLn "hi"

parseTypeScript :: String -> Either ParseError [DeclarationElement]
parseTypeScript = runIdentity . runParserT declarationSourceFile () ""

parseFile :: String -> IO (Either ParseError [DeclarationElement])
parseFile = liftM parseTypeScript . readFile

compile :: String -> String
compile = id