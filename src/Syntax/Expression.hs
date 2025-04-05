module Syntax.Expression where
import qualified Data.Map as M

--data Expression = Var String | Application Expression Expression | Lambda String Expression | Definition String Expression deriving (Show, Eq)

data Expression 
    = Var String
    | Lambda String Expression
    | Application Expression Expression
    | Definition String Expression
    deriving Eq

instance Show Expression where
    show = showExpr
    
-- Helper function to show expressions in lambda calculus notation
showExpr :: Expression -> String
showExpr (Var x) = x
showExpr (Lambda x e) = "\\" ++ x ++ "." ++ showExpr e
showExpr (Application e1 e2) = "(" ++ showExpr e1 ++ " " ++ showExpr e2 ++ ")"
showExpr (Definition name expr) = name ++ "=" ++ showExpr expr

type Context = M.Map String Expression
