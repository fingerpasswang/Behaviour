{-# LANGUAGE BangPatterns #-}

module CodeGenerator where

import Common
import Parser
import qualified Data.Map    as Map
import           Data.Maybe
import           Debug.Trace
import System.IO (Handle, hSetEncoding, stdout, utf8)
import qualified Data.ByteString.Char8 as C
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec 

merge_dec_env :: Maybe DecEnv -> Maybe DecEnv -> Maybe DecEnv
merge_dec_env menv0 menv1 = menv0 >>= (\env0 -> menv1 >>= (\env1 -> return (Map.union env0 env1)))

make_dec_env :: String -> Maybe (String, Dec) -> Maybe DecEnv
make_dec_env var msig = msig >>= (\sig -> return (Map.singleton var sig))

handle_dec :: Dec -> DecEnv -> String -> IO (Maybe DecEnv)
handle_dec (DeclaresDec ds) env currentModule = (foldr folder (return (Just env)) ds) >>= (\env_agg -> return env_agg)
	where
		folder dec io_res = do
			result <- io_res
			case result of 
				(Just env) -> handle_dec dec env currentModule
				Nothing -> return Nothing
handle_dec (DeclareDec (VarPat var) sig) env currentModule = return (merge_dec_env (Just env) (make_dec_env var (Just (currentModule, (DeclareDec (VarPat var) sig)))))
handle_dec (DefineDec (VarPat var) exp) env currentModule = 
	return (merge_dec_env 
		(Just env) 
		(make_dec_env var (Just (currentModule, (DefineDec (VarPat var) exp)))))
handle_dec (ImportDec moduleName) env currentModule = do
	importEnv <- load_dec_env moduleName
	return (merge_dec_env (Just env) importEnv)

load_dec_env :: String -> IO (Maybe DecEnv)
load_dec_env currentModule = do
    context <- C.readFile (currentModule ++ ".bh")
    if C.null context
    then return Nothing
    else 
		do
			result <- (test_gen (parse p_main "" (C.unpack context)) currentModule) 
			return (snd result)
		
test_gen :: (Either ParseError [Dec]) -> String -> IO (String, Maybe DecEnv)
test_gen (Right decs) currentModule = foldl folder (return ("begin:", (Just Map.empty))) decs
	where
		folder io_res dec = 
			do 
				result <- io_res
				case result of 
					(output, (Just env)) -> do
						union_res <- handle_dec dec env currentModule
						case union_res of 
							Just env1 -> return (output ++ show dec ++ "complete\n", Just env1)
							Nothing -> return (output ++ show dec ++ "failed\n", Nothing)
					(output, Nothing) -> return (output, Nothing)		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	