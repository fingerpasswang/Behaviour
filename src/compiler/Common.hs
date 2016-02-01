module Common where

import qualified Data.Map as Map

type Identifier = String
type ValEnv = Map.Map Identifier Val
type TypeEnv = Map.Map Identifier Type
type DecEnv = Map.Map Identifier (String,Dec)

data Type = 
	NormalType String
	| GenericType String Type
	| AppType [Type]
		deriving (Eq, Ord, Show)

data Dec =
    DefineDec Pat Exp
    | ImportDec String
	| DeclareDec Pat Type
	| DeclaresDec [Dec]
		deriving (Eq, Ord, Show)
		
data Exp = 
	ConstExp Val
	| VarExp Identifier
	| LambdaExp Pat Exp
	| AppExp Exp Exp
	| ADTExp String [Exp]
		deriving (Eq, Ord, Show)
		
data Val =
    NilVal
    | BoolVal Bool
    | IntVal Integer
    | FloatVal Float
    | StringVal String
	| ClosureVal Pat Exp ValEnv
	| ADTVal String [Val]
		deriving (Eq, Ord, Show)
	
data Pat =
    NilPat
    | ConstPat Val
    | VarPat Identifier
	| ADTPat String [Pat]
		deriving (Eq, Ord, Show)
		
