module Main where

import Syntax.Grammar
import Evaluation.Normal
import Evaluation.Applicative
import Evaluation.Substitution

main :: IO ()
main = readFile "../prog.txt"
     >>= maybe (putStrLn "Syntax error!") (mapM_ print) . parseProgram
