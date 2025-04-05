module Evaluation.Applicative where
import Syntax.Expression
import Evaluation.Substitution
import Syntax.Grammar
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

internal :: String -> Expression
internal = maybe (error "Syntax error!") head . parseProgram

makeContext :: [(String, String)] -> Context
makeContext = M.fromList . map (fmap internal)

empty :: Context
empty = makeContext []

-- | Type for evaluation with error handling and IO capabilities
type EvalWithError a = ExceptT String (StateT Context IO) a

-- Track evaluation steps to prevent infinite loops
maxSteps :: Int
maxSteps = 100  -- Reasonable limit for most expressions

-- Global counter for steps
stepsCounter :: IORef Int
stepsCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE stepsCounter #-}

{-|
    Check if an expression is a value (cannot be reduced further)
-}
isValue :: Expression -> Bool
isValue (Lambda _ _) = True     -- Lambda expressions are values
isValue (Var _) = True          -- Free variables are considered values
isValue _ = False

-- Reset step counter
resetSteps :: IO ()
resetSteps = writeIORef stepsCounter 0

-- Increment step counter and check limit
incrementAndCheckSteps :: EvalWithError ()
incrementAndCheckSteps = do
    steps <- liftIO $ readIORef stepsCounter
    let newSteps = steps + 1
    liftIO $ writeIORef stepsCounter newSteps
    when (newSteps >= maxSteps) $
        throwError "Evaluation exceeded maximum steps - potential infinite loop detected"

{-|
    Single step of applicative-order evaluation
-}
evalStep :: Expression -> EvalWithError (Maybe Expression)
evalStep expr = do
    incrementAndCheckSteps
    case expr of
        -- Variable lookup from context
        Var x -> do
            ctx <- lift get
            case M.lookup x ctx of
                Just e -> return (Just e)        -- Replace with value from context
                Nothing -> return Nothing         -- Free variable remains unchanged (value form)
       
        -- Lambda abstractions are values
        Lambda _ _ -> return Nothing
       
        -- For application, evaluate according to applicative order rules
        Application e1 e2 ->
            -- First check if argument needs evaluation (applicative order)
            if not (isValue e2) then do
                e2Result <- evalStep e2
                case e2Result of
                    Just e2' -> return (Just (Application e1 e2'))
                    Nothing -> evalFunctionPos e1 e2
            else evalFunctionPos e1 e2
       
        -- Definition updates the context with the expression (but doesn't evaluate it yet)
        Definition x e -> do
            lift $ modify (M.insert x e)
            return (Just e)

-- Helper to evaluate the function position in an application
evalFunctionPos :: Expression -> Expression -> EvalWithError (Maybe Expression)
evalFunctionPos e1 e2 = case e1 of
    -- If function is a variable, try to look it up
    Var x -> do
        ctx <- lift get
        case M.lookup x ctx of
            Just e -> return (Just (Application e e2))
            Nothing -> return Nothing  -- Can't reduce further

    -- If function is a lambda, perform substitution
    Lambda x body -> return (Just (subst x e2 body))

    -- Otherwise evaluate the function position
    _ -> do
        e1Result <- evalStep e1
        case e1Result of
            Just e1' -> return (Just (Application e1' e2))
            Nothing -> return Nothing  -- Can't reduce further

{-|
    Single-step evaluation with monadic interface
-}
eval :: Expression -> Context -> IO (Either String Expression, Context)
eval expr ctx = do
    resetSteps  -- Reset step counter before evaluation
    runStateT (runExceptT $ do
        result <- evalStep expr
        case result of
            Just newExpr -> return newExpr
            Nothing -> return expr) ctx

{-|
    Multi-step evaluation to normal form using applicative order
    with error handling for infinite loops
-}
evalM :: Expression -> EvalWithError Expression
evalM expr = do
    result <- evalStep expr
    case result of
        Just newExpr -> 
            if newExpr == expr
            then return expr  -- Prevent loops where substitution doesn't change the expression
            else evalM newExpr  -- Continue evaluating
        Nothing -> return expr  -- Value form reached

{-|
    Full evaluation with the requested type signature
-}
evalFull :: Expression -> Context -> IO (Either String Expression, Context)
evalFull expr ctx = do
    resetSteps  -- Reset step counter before evaluation
    runStateT (runExceptT $ evalM expr) ctx

-- Special handling for the Y combinator when applied
-- This returns a partially evaluated result that's useful to the programmer
handleYCombinator :: Expression -> Context -> IO (Either String Expression, Context)
handleYCombinator expr ctx = 
    case expr of
        Application (Var "fix") arg -> do
            -- Look up the fix definition
            case M.lookup "fix" ctx of
                Just fixDef -> do
                    -- Apply one step of expansion but don't recurse further
                    let expanded = Application 
                          (Lambda "x" (Application arg (Application (Var "x") (Var "x"))))
                          (Lambda "x" (Application arg (Application (Var "x") (Var "x"))))
                    return (Right expanded, ctx)
                Nothing -> evalFull expr ctx
        _ -> evalFull expr ctx

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
            (result, newCtx) <- evalFull expr ctx
            case result of
                Left errMsg -> do
                    putStrLn $ "Error: " ++ errMsg
                    processExpressions exprs ctx
                Right evaluatedExpr -> do
                    -- Print the evaluated expression without debugging info
                    print evaluatedExpr
                    processExpressions exprs newCtx
