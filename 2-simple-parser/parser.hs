-- Requires Parsec (cabal install parsec)
-- 14 Jun 2014

-- We'll be defining our own `spaces` later
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- Define a parser that recognizes one of the symbols allowed
-- in Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Define a function to call our parser and handle any possible errors
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    -- parse returns an Either: left for error, right for a value
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
