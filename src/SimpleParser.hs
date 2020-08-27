{- Example to show why Haskell is good for parsing
with a simple parser for expressions 
of the form "((1 * (2 - 3) + (4 + 5))" 
-}
module SimpleParser where

import Text.Trifecta
import Control.Applicative

-- | data type to represent the structure we want to parse
data Expr -- an expression can be one of these:
    = Add Expr Expr -- addition of expressions
    | Sub Expr Expr -- subtraction of expressions
    | Mul Expr Expr -- multiplication of expressions
    | Lit Integer   -- a literal number, an integer
    deriving (Show)


-- this is how we evaluate the expression
-- recursively and pattern matching on the types
evalExpr :: Expr -> Integer
evalExpr (Add x y) = (evalExpr x) + (evalExpr y)
evalExpr (Sub x y) = (evalExpr x) - (evalExpr y)
evalExpr (Mul x y) = (evalExpr x) * (evalExpr y)
evalExpr (Lit x) = x

-- main expression parser
exprParser :: Parser Expr
exprParser 
    =   try addParser
    <|> try subParser
    <|> try mulParser
    <|> litParser

-- parser for addition
addParser :: Parser Expr
addParser = parens $ do
    a <- exprParser
    _ <- symbolic '+'
    b <- exprParser
    pure (Add a b)

-- parser for subtraction
subParser :: Parser Expr
subParser = parens $ do
    a <- exprParser
    _ <- symbolic '-'
    b <- exprParser
    pure (Sub a b)

-- parser for multiplication
mulParser :: Parser Expr
mulParser = parens $ do
    a <- exprParser
    _ <- symbolic '*'
    b <- exprParser
    pure (Mul a b)

-- parser for integer literals
litParser :: Parser Expr
litParser = Lit <$> integer

-- run any of the parsers above on a string
runExprParser :: Parser Expr -> String -> Result Expr 
runExprParser = flip parseString mempty

-- run the top level parser
parseExpr :: String -> Result Expr
parseExpr = runExprParser exprParser

