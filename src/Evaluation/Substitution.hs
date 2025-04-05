module Evaluation.Substitution where
import Syntax.Expression

{-|
    Returns the list of free variables in an expression.
-}
freeVars :: Expression -> [String]
freeVars (Var x) = [x]
freeVars (Application e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Lambda x e) = filter (/= x) (freeVars e)
freeVars (Definition name expr) = name : freeVars expr

{-|
    Substitutes expression 'new' for all free occurrences of variable 'x' in 'expr'.
    Performs alpha-conversion when necessary to avoid variable capture.
-}
subst :: String -> Expression -> Expression -> Expression
subst x new (Var y)
    | x == y    = new
    | otherwise = Var y
   
subst x new (Application e1 e2) =
    Application (subst x new e1) (subst x new e2)
   
subst x new (Lambda y e)
    | x == y = Lambda y e  -- Don't substitute bound variable
    | y `notElem` freeVars new =
        Lambda y (subst x new e)  -- Safe to substitute
    | otherwise =
        -- Need alpha conversion to avoid capture
        let y' = freshVar y (freeVars new ++ freeVars e)
            e' = subst y (Var y') e
        in Lambda y' (subst x new e')

{-|
    Generates a fresh variable name that doesn't appear in the given list of used names.
-}
freshVar :: String -> [String] -> String
freshVar x used 
    | x `notElem` used = x
    | otherwise = x ++ "#"  -- Simple append of # for renamed variables

{-|
    Check if a variable occurs free in an expression.
-}
occursFree :: String -> Expression -> Bool
occursFree x = elem x . freeVars

{-|
    Check if a substitution would cause variable capture.
-}
isSafeSubst :: String -> Expression -> Expression -> Bool
isSafeSubst x new expr = all (`notElem` freeVars new) (boundVars expr)
  where
    boundVars (Lambda y e) = y : boundVars e
    boundVars (Application e1 e2) = boundVars e1 ++ boundVars e2
    boundVars (Definition name e) = name : boundVars e
    boundVars (Var _) = []