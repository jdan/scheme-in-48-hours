-- Requires Parsec (cabal install parsec)
-- 14 Jun 2014

-- We'll be defining our own `spaces` later
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

import LispVal

-- Define a parser that recognizes one of the symbols allowed
-- in Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Define a parser that recognizes any number of whitespace characters
spaces :: Parser ()
spaces = skipMany1 space

-- Some more parsers
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

-- Define a function to call our parser and handle any possible errors
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    -- parse returns an Either: left for error, right for a value
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
