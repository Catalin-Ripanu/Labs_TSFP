{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Evaluation.NormalWithErrorTest where

import Evaluation.Normal
import Syntax.Expression
import qualified Data.Map as M
import Test.Framework
import Control.Arrow (first)
import Util

test_evalWithError :: IO ()
test_evalWithError = do
    -- Test undefined variable (should return Left with error message)
    result1 <- evalInternal "x" empty
    assertEqual (Left "Undefined variable: x", empty) result1

    -- Test defined variable
    let context = makeContext [("x", "y")]
    result2 <- evalInternal "x" context
    assertEqual (Right "y", context) result2

    -- Test lambda (should succeed)
    result3 <- evalInternal "\\x.x" empty
    assertEqual (Right "\\x.x", empty) result3

    -- Test beta reduction
    result4 <- evalInternal "(\\x.x y)" empty
    assertEqual (Right "y", empty) result4

    -- Test context modification with definition
    result5 <- evalInternal "id=\\x.x" empty
    assertEqual (Right "\\x.x", makeContext [("id", "\\x.x")]) result5

    -- Test application with defined function
    let idContext = makeContext [("id", "\\x.x")]
    result6 <- evalInternal "(id y)" idContext
    assertEqual (Right "(\\x.x y)", idContext) result6

    -- Test normal-order specific case
    result7 <- evalInternal "(\\x.y (\\x.(x x) \\x.(x x)))" empty
    assertEqual (Right "y", empty) result7
  where
    evalInternal :: String -> Context -> IO (Either String String, Context)
    evalInternal expr context = do
        (result, newContext) <- eval (internal expr) context
        return (fmap show result, newContext)
    empty = makeContext [] 