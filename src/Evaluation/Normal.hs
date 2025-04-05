module Evaluation.Normal where
import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M
import Control.Monad.State
import Syntax.Grammar
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.Functor.Identity

{-|
    Small-step normal-order evaluation of a given expression,
    within a given context.
-}

internal :: String -> Expression
internal = maybe (error "Syntax error!") head . parseProgram

makeContext :: [(String, String)] -> Context
makeContext = M.fromList . map (fmap internal)

empty :: Context
empty = makeContext []

-- | Type synonym for evaluation monad with error handling and IO capabilities
type EvalWithError a = ExceptT String (StateT Context IO) a

-- Fixed version of evalM - performs full normal-order evaluation
evalM :: Expression -> EvalWithError Expression
evalM expr = do
    result <- evalStep expr
    case result of
        Just newExpr -> evalM newExpr  -- Continue evaluating
        Nothing -> return expr         -- Value form reached

-- | Single step of evaluation
evalStep :: Expression -> EvalWithError (Maybe Expression)
evalStep expr = case expr of
    -- Variables: lookup in context
    Var x -> do
        ctx <- lift get
        case M.lookup x ctx of
            Just e -> return (Just e)
            Nothing -> return Nothing  -- Free variable is a value
    
    -- Lambda is already a value
    Lambda _ _ -> return Nothing
    
    -- Application: evaluate function first (normal order)
    Application e1 e2 -> case e1 of
        -- If e1 is lambda, substitute unevaluated e2
        Lambda x body -> return (Just (subst x e2 body))
        
        -- If e1 isn't lambda yet, evaluate it first
        _ -> do
            e1Result <- evalStep e1
            case e1Result of
                Just e1' -> return (Just (Application e1' e2))
                Nothing -> return Nothing  -- Can't reduce further
    
    -- Definition: store expression and return it
    Definition x e -> do
        lift $ modify (M.insert x e)
        return (Just e)

-- Main evaluation function
eval :: Expression -> Context -> IO (Either String Expression, Context)
eval expr ctx = runStateT (runExceptT (evalM expr)) ctx

-- Reimplemented evaluate function for fully evaluating expressions
evaluate :: IO ()
evaluate = do
    content <- readFile "../prog.txt"
    case parseProgram content of
        Nothing -> putStrLn "Syntax error!"
        Just exprs -> processExpressions exprs empty
    where
        processExpressions :: [Expression] -> Context -> IO ()
        processExpressions [] _ = return ()
        processExpressions (expr:exprs) ctx = do
            -- First evaluate the expression fully
            (result, newCtx) <- eval expr ctx
            case result of
                Left errMsg -> do
                    putStrLn $ "Error: " ++ errMsg
                    processExpressions exprs ctx
                Right evaluatedExpr -> do
                    -- Print the evaluated expression without debugging info
                    print evaluatedExpr
                    processExpressions exprs newCtx
        

