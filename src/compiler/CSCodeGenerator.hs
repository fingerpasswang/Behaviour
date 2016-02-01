{-# LANGUAGE BangPatterns #-}

module CSCodeGenerator where

import Common
import Parser
import CodeGenerator
import TypeChecker
import qualified Data.Map    as Map
import           Data.Maybe
import           Debug.Trace
import System.IO (Handle, hSetEncoding, stdout, utf8)
import qualified Data.ByteString.Char8 as C
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec 
import Text.Format

dec_to_code :: Dec -> TypeEnv -> DecEnv -> Maybe String
dec_to_code (DefineDec (VarPat var) exp) typeEnv decEnv = do
	t <- Map.lookup var typeEnv
	exp_code <- exp_to_code exp typeEnv decEnv
	return (format "public static {0} Root = {1}" [(type_to_code t), exp_code])
dec_to_code _ _ _ = Just ""

decs_to_code :: [Dec] -> TypeEnv -> DecEnv -> Maybe String
decs_to_code decs typeEnv decEnv = foldl (inner typeEnv decEnv) (Just "") decs
	where
		inner typeEnv decEnv (Just str) dec = do
			dec_code <- dec_to_code dec typeEnv decEnv
			return (str ++ (dec_code))
			
type_to_code :: Type -> String
type_to_code (NormalType typeName) = typeName
type_to_code (GenericType generic t) = generic ++ "<" ++ type_to_code t ++ ">"

exp_to_code :: Exp -> TypeEnv -> DecEnv -> Maybe String
exp_to_code (AppExp (VarExp var) exp) typeEnv decEnv = do
	(moduleName,dec) <- Map.lookup var decEnv
	exp_code <- exp_to_code exp typeEnv decEnv
	return (format "{0}.{1}({2})" [moduleName, var, exp_code])
exp_to_code (AppExp (LambdaExp (VarPat local) appexp) exp) typeEnv decEnv = do
	t <- exp_type exp typeEnv
	get_exp_code <- exp_to_code exp typeEnv decEnv
	sub_exp_code <- exp_to_code appexp typeEnv decEnv
	return (format "(((Box<{0}> {1}) => Help.With({1}, {2}, {3}))(new Box<{0}>()))" [(type_to_code t), local, get_exp_code, sub_exp_code]) 
exp_to_code (ADTExp "Cons" exps) typeEnv decEnv = do
	inner_code <- inner exps typeEnv decEnv
	return (format "Help.MakeList({0})" [inner_code])
	where
		inner (e:es) tEnv dEnv = do
			case (head es) of 
				ConstExp _ -> exp_to_code e tEnv dEnv
				ADTExp "Cons" exps -> do
					exp_code <- exp_to_code e tEnv dEnv
					code <- inner exps tEnv dEnv
					return (format "{0},{1}" [exp_code, code])
exp_to_code (VarExp var) typeEnv decEnv = 
	case (Map.lookup var decEnv) of 
		(Just (moduleName, dec)) -> Just (format "{0}.{1}" [moduleName, var])
		_ -> Just ""
			
test_cs_decs :: String -> IO (([Dec], TypeEnv, DecEnv))
test_cs_decs moduleName = do 
	context <- C.readFile (moduleName ++ ".bh")
	if C.null context
	then return ([],(Map.singleton "" (NormalType "")), (Map.singleton "" ("", (ImportDec ""))))
	else 
		do
			let parse_result = parse p_main "" (C.unpack context)
				in
					do
						(_, Just typeEnv) <- test_check parse_result
						(_, Just decEnv) <- test_gen parse_result moduleName
						case parse_result of 
							(Right decs) -> do
								return (decs,typeEnv,decEnv)

test_cs_gen :: String -> IO ()
test_cs_gen moduleName = do
	context <- C.readFile (moduleName ++ ".bh")
	if C.null context
	then putStrLn $ moduleName ++ " is not exist"
	else 
		do
			let parse_result = parse p_main "" (C.unpack context)
				in
					do
						(_, Just typeEnv) <- test_check parse_result
						(_, Just decEnv) <- test_gen parse_result moduleName
						case parse_result of 
							(Right decs) -> do
--								putStrLn (show typeEnv)
--								putStrLn (show decEnv)
--								putStrLn (show decs)
								putStrLn $ show (decs_to_code decs typeEnv decEnv)

					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					
					