{-# LANGUAGE BangPatterns #-}

module Parser where

import Common
import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Char
import Numeric (readHex, readDec, readSigned, readFloat, readInt)
import Control.Monad (liftM2, liftM3, liftM4)
import System.IO (Handle, hSetEncoding, stdout, utf8)
import Data.Char(isLower, isUpper, isAlpha, isAlphaNum)
import qualified Data.ByteString.Char8 as C
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)

--newtype Parser = CharParser ()

-- 辅助用 begin
listplus :: [Parser a] -> Parser a
listplus lst = foldr (<|>) mzero (map try lst)

star   :: Parser a -> Parser [a]
star p = star_p
	where 
		star_p = try plus_p <|> (return []) 
		plus_p = (:) <$> p <*> star_p 
		
plus   :: Parser a -> Parser [a]
plus p = plus_p
    where
        star_p = try plus_p <|> (return []) <?> "plus_star_p"
        plus_p = (:) <$> p <*> star_p  <?> "plus_plus_p"
		
p_junk :: Parser ()
p_junk =  spaces 

p_parse   :: Parser a -> Parser a
p_parse p =  p_junk *> p

p_token   :: Parser a -> Parser a
p_token p =  p <* p_junk
			 <?> "p_token"
			 
p_seperate      :: Parser a -> String -> Parser [a]
p_seperate p sep =  (:) 
					<$> p
					<*> star (p_parse p_rest)
	where p_rest =   (p_parse (string sep)) *> p
	
p_seperate1      :: Parser a -> String -> Parser [a]
p_seperate1 p sep =  (:) 
					<$> p
					<*> plus (p_parse p_rest)
	where p_rest =   (p_parse (string sep)) *> p
	
p_between 		:: Char -> Char -> Parser a -> Parser a
p_between l r p =   char l 
				 *>	(spaces *> p <* spaces)
				<*  char r
				<?> "p_between"
				
p_word :: Parser Identifier
p_word =  p_token varname 
	where varname = (:) <$> satisfy isAlpha
						<*> many (listplus [(satisfy isAlphaNum), (char '_')])
						<?> "p_word"

p_consname :: Parser String
p_consname =  p_token consname 
	where consname = (:) <$> satisfy isUpper
						<*> many (listplus [(satisfy isAlphaNum), (char '_'), (char '.')])
						<?> "p_consname"
						
exclude_last :: [a] -> [a]		
exclude_last []			= []
exclude_last (f:[]) 	= []
exclude_last (f:rest) 	= f : (exclude_last rest)

--p_type :: Parser [String]
--p_type =  (plus p_word)
--				<?> "p_type"
			
p_float :: Parser Float
p_float  = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty
		   <?> "p_float"
	
p_int :: Parser Integer
p_int  = do s <- getInput
            case readSigned readDec s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty
		 <?> "p_int"
		 			
-- 辅助用 end

-- type begin
p_normal_type :: Parser Type
p_normal_type =  NormalType
			     <$> p_parse p_word
				 <?> "p_normal_type"
				 
p_generic_type :: Parser Type
p_generic_type =  GenericType
				  <$> p_parse p_word
				  <*> p_parse p_type
				  <?> "p_generic_type"
	where
		generic_list = listplus (map string ["List"])

p_app_type :: Parser Type
p_app_type =  p_between '(' ')' inner
			 <?> "p_app_type"	
	where
		inner = AppType
			    <$> p_seperate1 (p_parse non_app_type) "->"
		non_app_type = listplus [p_generic_type, p_normal_type]
			  
p_type :: Parser Type
p_type =  listplus [p_app_type, p_generic_type, p_normal_type]
-- type end

-- val begin
p_nil_val :: Parser Val
p_nil_val =  NilVal 
			  <$  string "nil"
			  <?> "p_nil_val"
		 
p_bool_val :: Parser Val
p_bool_val =  BoolVal 
			  <$> listplus [true_p, false_p]
			  <?> "p_bool_val"
	where 
		true_p  = True <$ (string "True")
		false_p = False <$ (string "False") 
		
p_int_val :: Parser Val
p_int_val =  IntVal 
			 <$> p_int 
			 <?> "p_int_val"

p_float_val :: Parser Val
p_float_val =  FloatVal 
			   <$> p_float 
			   <?> "p_float_val"

p_string_val :: Parser Val
p_string_val =  StringVal 
				<$> (p_between '\"' '\"' inner)
				<?> "p_string_val"
	where
		inner = star (satisfy (\c -> c /= '"')) 
		
p_adt_val :: Parser Val
p_adt_val =  p_between '(' ')' inner
			 <?> "p_adt_val"	
	where
		inner =  ADTVal
				 <$> p_parse p_consname
				 <*> star (p_parse p_val)
				 <?> "p_adt_val"
			 
p_val :: Parser Val
p_val =  listplus [p_nil_val,p_bool_val,p_int_val,p_float_val,p_string_val,p_adt_val]
-- val end

-- dec begin
p_define_dec :: Parser Dec
p_define_dec =  p_between '(' ')' inner
				<?> "p_define_dec"	
	where
		inner = DefineDec 
				<$  string "define"
				<*> p_parse p_pat
				<*> p_parse p_exp
				
p_import_dec :: Parser Dec
p_import_dec =  p_between '(' ')' inner
				<?> "p_import_dec"	
	where
		inner = ImportDec
				<$  string "import"
				<*> p_parse p_word
				
p_declares_dec :: Parser Dec
p_declares_dec =  p_between '(' ')' inner
				  <?> "p_declares_dec"
	where
		inner = DeclaresDec
				<$  p_parse (string "declare")
				<*> star (p_parse p_declare_dec)

p_declare_dec :: Parser Dec
p_declare_dec =  p_between '(' ')' inner
				  <?> "p_declare_dec"
	where
		inner = DeclareDec 
				<$> p_parse p_pat
				<*  p_parse (string "::")
				<*> p_parse p_type
			
p_dec :: Parser Dec
p_dec =  listplus [p_define_dec,p_import_dec,p_declares_dec,p_declare_dec]
-- dec end

-- exp begin
p_var_exp :: Parser Exp
p_var_exp =  VarExp
			 <$> p_parse p_word
			 <?> "p_var_exp"
			 
p_const_exp :: Parser Exp
p_const_exp =  ConstExp
			   <$> p_parse p_val
			   <?> "p_const_exp"

p_lambda_exp :: Parser Exp
p_lambda_exp =  p_between '(' ')' inner
			  <?> "p_lambda_exp"
	where
		inner = make_lambda_exp
				<$  char '\\'
--				<*> plus (p_parse p_pat)
				<*> p_seperate (p_parse p_pat) ","
				<*> p_parse p_exp
		make_lambda_exp []     e = (LambdaExp NilPat e)
		make_lambda_exp (p:[]) e = (LambdaExp p e)
		make_lambda_exp (p:ps) e = (LambdaExp p (make_lambda_exp ps e))
		
p_app_exp :: Parser Exp
p_app_exp =  p_between '(' ')' inner
			  <?> "p_app_exp"
	where
		inner = make_app_exp
			    <$>  p_parse p_exp
			    <*> plus (p_parse p_exp)
			    <?> "p_app_exp"
		make_app_exp acc (e:[]) = AppExp acc e
		make_app_exp acc (e:es) = make_app_exp (AppExp acc e) es

p_adt_exp :: Parser Exp
p_adt_exp =  p_between '(' ')' inner
			  <?> "p_adt_exp"
	where
		inner = ADTExp 
			    <$> p_parse p_word
			    <*> star (p_parse p_exp)
			 
p_list_exp :: Parser Exp
p_list_exp =  p_between '[' ']' inner
			  <?> "p_list_exp"
	where
		inner = make_list_exp 
			    <$> star (p_parse p_exp)
		make_list_exp [] = ConstExp NilVal
		make_list_exp (e:es) = ADTExp "Cons" [e, (make_list_exp es)]

p_exp :: Parser Exp	
p_exp =  listplus [p_var_exp, p_const_exp, p_lambda_exp, p_app_exp, p_adt_exp, p_list_exp]
		 <?> "p_exp"

-- exp end

-- pat begin
p_nil_pat :: Parser Pat
p_nil_pat =  NilPat 
			 <$  char '_'
			 <?> "p_nil_pat"

p_const_pat :: Parser Pat
p_const_pat =  ConstPat
			   <$> p_val
			   <?> "p_const_pat"

p_var_pat :: Parser Pat
p_var_pat =  VarPat 
			 <$> p_word
			 <?> "p_var_pat"

p_adt_pat :: Parser Pat
p_adt_pat =  p_between '(' ')' inner
			 <?> "p_adt_pat"	
	where
		inner = ADTPat
				<$> p_parse p_consname
				<*> star (p_parse p_pat)
			
p_pat :: Parser Pat
p_pat = listplus [p_nil_pat,p_var_pat,p_const_pat, p_adt_pat]
		<?> "p_pat"
-- pat end		
			
p_main :: Parser [Dec]
p_main = plus (p_token (p_parse p_dec))

parse_main :: FilePath -> IO ()
parse_main file = do
    context <- C.readFile file
    if C.null context
    then putStrLn $ file ++ " is not exist"
    else putStrLn $ show (parse p_main "" (C.unpack context))






