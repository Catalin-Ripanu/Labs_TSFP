{-# LANGUAGE FlexibleContexts #-}
module Evaluation.Big where
import Syntax.Expression
import Control.Monad.State
import Data.List (mapAccumL)
import Data.Tuple (swap)
import qualified Data.Map as M

{-|
    Big-step evaluation of a given expression, within a given context.
    Repeatedly applies small-step evaluation until reaching normal form.
-}

type Eval a = State Context a  -- or type Eval = State Context

-- Monadic evaluators
evalBigM :: (Expression -> Eval Expression) -> Expression -> Eval Expression
evalBigM step expr = do
    e <- step expr
    if expr == e
        then pure e
        else evalBigM step e

evalListM :: (Expression -> Eval Expression) -> [Expression] -> Eval [Expression]
evalListM = mapM . evalBigM

-- Non-monadic wrappers
evalBig :: (Expression -> Context -> (Expression, Context))  
        -> Expression
        -> Context
        -> (Expression, Context)
evalBig step = runState . evalBigM (\e -> state (step e))

evalList :: (Expression -> Context -> (Expression, Context))
         -> [Expression]
         -> Context
         -> ([Expression], Context)
evalList step = runState . evalListM (\e -> state (step e))
