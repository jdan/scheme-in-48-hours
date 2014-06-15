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
-- liftM so we can use (Number . read) on a many1
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- Define a function to call our parser and handle any possible errors
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    -- parse returns an Either: left for error, right for a value
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ show val


-- Time to evaluate (may want to refactor this)
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

-- "point free" style
-- writing definitions purely in terms of function composition
-- and partial application
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- add showVal as an instance method to the Show class
instance Show LispVal where show = showVal

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
