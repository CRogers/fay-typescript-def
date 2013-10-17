module Main where

import Control.Monad
import Data.Functor.Identity
import Text.Parsec
import qualified Language.TypeScript as T
import qualified Language.Haskell.Exts.Syntax as H
import Language.Haskell.Exts.Pretty (prettyPrint)

main = putStrLn "hi"

parseTypeScript :: String -> Either ParseError [T.DeclarationElement]
parseTypeScript = runIdentity . runParserT T.declarationSourceFile () ""

parseFile :: String -> IO (Either ParseError [T.DeclarationElement])
parseFile = liftM parseTypeScript . readFile

compile :: String -> String
compile = id

mapPredefType :: T.PredefinedType -> H.Type
mapPredefType predef = H.TyCon . H.UnQual . H.Ident $
	case predef of
		T.AnyType -> error "any type"
		T.NumberType -> "Float"
		T.BooleanType -> "Boolean"
		T.StringType -> "String"
		T.VoidType -> "()"

sl :: H.SrcLoc
sl = H.SrcLoc "" 0 0

makeFFICall :: String -> H.Exp
makeFFICall name = H.App (H.Var $ H.UnQual (H.Ident "ffi")) (H.Lit $ H.String name)

makeFFI :: String -> H.Type -> [H.Decl]
makeFFI name ty = [typeSig, func]
	where
		typeSig = H.TypeSig sl [H.Ident name] ty
		func = H.PatBind sl (H.PVar $ H.Ident name) Nothing (H.UnGuardedRhs $ makeFFICall name) (H.BDecls [])

prettyPrintFFI :: String -> String
prettyPrintFFI = prettyPrint . makeFFICall