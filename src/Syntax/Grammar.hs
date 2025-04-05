module Syntax.Grammar where
import Syntax.Expression
import Syntax.Parser
import Control.Applicative
import Data.Char (isSpace)

-- Skips whitespace characters
spaces :: Parser ()
spaces = many (spot isSpace) *> success ()

-- Parse one or more letters recursively
letters :: Parser String
letters = liftA2 (:) letter (letters <|> success [])

-- Parse a variable name
parseVar :: Parser Expression
parseVar = Var <$> letters

-- Parse a lambda abstraction: \x.expr
parseLambda :: Parser Expression
parseLambda = liftA2 Lambda
    (token '\\' *> letters)
    (token '.' *> parseSimpleExpr)

-- Parse an application: (expr1 expr2)
parseApplication :: Parser Expression
parseApplication = liftA2 Application
    (token '(' *> parseSimpleExpr)
    (parseSimpleExpr <* token ')')

-- Parse basic expressions (no definitions)
parseSimpleExpr :: Parser Expression
parseSimpleExpr = spaces *> (parseLambda <|> parseApplication <|> parseVar) <* spaces

-- Parse a definition: x=expr
parseDefinition :: Parser Expression
parseDefinition = liftA2 Definition
    letters
    (token '=' *> parseSimpleExpr)

-- Parse either a definition or a simple expression
parseExpr :: Parser Expression
parseExpr = spaces *> (parseDefinition <|> parseSimpleExpr) <* spaces

-- Parse multiple expressions and definitions
parseLines :: Parser [Expression]
parseLines = liftA2 (:) (parseExpr <* spaces) parseLines
        <|> success []

-- Main parsing function for programs
parseProgram :: String -> Maybe [Expression]
parseProgram input = parse (parseLines <* eof) input
