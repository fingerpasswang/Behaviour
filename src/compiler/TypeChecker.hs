{-# LANGUAGE BangPatterns #-}

module TypeChecker where

import Common
import Parser
import qualified Data.Map    as Map
import           Data.Maybe
import           Debug.Trace
import System.IO (Handle, hSetEncoding, stdout, utf8)
import qualified Data.ByteString.Char8 as C
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec 

merge_type_env :: Maybe TypeEnv -> Maybe TypeEnv -> Maybe TypeEnv
merge_type_env menv0 menv1 = menv0 >>= (\env0 -> menv1 >>= (\env1 -> return (Map.union env0 env1)))

make_type_env :: String -> Maybe Type -> Maybe TypeEnv
make_type_env var msig = msig >>= (\sig -> return (Map.singleton var sig))


-- union begin
union_dec :: Dec -> TypeEnv -> IO (Maybe TypeEnv)
union_dec (DeclaresDec ds) env = (foldr folder (return (Just env)) ds) >>= (\env_agg -> return env_agg)
	where
		folder dec io_res = do
			result <- io_res
			case result of 
				(Just env) -> union_dec dec env
				Nothing -> return Nothing
union_dec (DeclareDec (VarPat var) sig) env = return (merge_type_env (Just env) (make_type_env var (Just sig)))
union_dec (DefineDec (VarPat var) exp) env = return (merge_type_env (Just env) (make_type_env var (exp_type exp env)))
union_dec (ImportDec moduleName) env = do
	importEnv <- load_env (moduleName ++ ".bh")
	return (merge_type_env (Just env) importEnv)

--union_dec (ImportDec moduleName) env = 
-- union end

-- exp_type begin
exp_type :: Exp -> TypeEnv -> Maybe Type
exp_type (ConstExp val) env = val_type val env
exp_type (VarExp var) env = Map.lookup var env
exp_type (LambdaExp _ exp) env = exp_type exp env
exp_type (AppExp lexp aexp) env = 
	(exp_type aexp env) >>= (\at -> 
		case lexp of 
			LambdaExp (VarPat var) exp -> (merge_type_env (Just env) (make_type_env var (Just at))) >>= (\env1 -> exp_type lexp env1)  
			_ -> (exp_type lexp env) >>= (\ltype -> check_type ltype at))
	where
		check_type (AppType (t1:(t2:[]))) at = 
			if t1 == at then (Just t2) else Nothing
		check_type (AppType (t:ts)) at = 
			if t == at then (Just (AppType ts)) else Nothing
		
exp_type (ADTExp "Cons" (e:es)) env = 
	case (head es) of 
		ConstExp _ -> 
			case (exp_type e env) of
				Just t -> Just (GenericType "List" t)
				Nothing -> Nothing
		ADTExp _ _ -> adt_type_check (exp_type e env) (exp_type (head es) env)
		where
			adt_type_check mt1 mt2 = 
				mt1 >>= (\t1 -> mt2 >>= (\t2 -> if ((GenericType "List" t1) == t2) then (Just t2) else Nothing)) 
-- exp_type end

-- val_type begin
val_type :: Val -> TypeEnv -> Maybe Type
val_type (BoolVal _) env = Just (NormalType "Bool")
val_type (IntVal _) env = Just (NormalType "Int")
val_type (FloatVal _) env = Just (NormalType "Float")
val_type (StringVal _) env = Just (NormalType "String")
--val_type (ClosureVal _ _ _) env = 
--val_type (ADTVal _ _) env = 

-- val_type end

load_env :: FilePath -> IO (Maybe TypeEnv)
load_env file = do
    context <- C.readFile file
    if C.null context
    then return Nothing
    else 
		do
			result <- (test_check (parse p_main "" (C.unpack context))) 
			return (snd result)
		
check_main :: TypeEnv -> Maybe Bool
check_main env = (Map.lookup "Root" env) >>= (\t -> return (t == (GenericType "IO" (NormalType "Result"))))
		
test :: FilePath -> IO ()
test file = do
    context <- C.readFile file
    if C.null context
    then putStrLn $ file ++ " is not exist"
    else 
		do
			result <- test_check (parse p_main "" (C.unpack context))
--			putStrLn $ show result
			putStrLn $ show ((snd result) >>= (\env -> check_main env))

test_check :: (Either ParseError [Dec]) -> IO (String, Maybe TypeEnv)
test_check (Right decs) = foldl folder (return ("begin:", (Just Map.empty))) decs
	where
		folder io_res dec = 
			do 
				result <- io_res
				case result of 
					(output, (Just env)) -> do
						union_res <- union_dec dec env
						case union_res of 
							Just env1 -> return (output ++ show dec ++ "complete\n", Just env1)
							Nothing -> return (output ++ show dec ++ "failed\n", Nothing)
					(output, Nothing) -> return (output, Nothing)

				





















		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		

