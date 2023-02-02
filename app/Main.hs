{-# LANGUAGE LambdaCase #-}
module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment
import Numeric (readOct, readHex, readFloat)
import Text.Parsec.Token (GenTokenParser(float))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | String String
             | Character Char
             | Bool Bool

main :: IO ()
main = do
        (expr:_) <- getArgs
        putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _  -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

escapeChars :: Parser Char
escapeChars = do
                char '\\'
                x <- oneOf "\\\"nrt"
                return $ case x of
                  '\\' -> x
                  '"'  -> x
                  'n'  -> '\n'
                  't'  -> '\t'
                  'r'  -> '\r'

oct2dig x = fst $ head (readOct x)
hex2dig x = fst $ head (readHex x)
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapeChars <|> noneOf "\""
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

parseBool :: Parser LispVal
parseBool = do
              char '#'
              (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseChar :: Parser LispVal
parseChar = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space")
            <|> do {x <- anyChar; notFollowedBy alphaNum; return [x]}
    return $ Character $ case value of
        "newline" -> '\n'
        "space"   -> ' '
        _         -> head value

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 
           <|> parseDecimal1
           <|> parseHex
           <|> parseOct
           <|> parseBin
           <|> try parseFloat


parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= return . Number . read

parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"
                   x <- many digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#h"
              x <- many hexDigit
              (return . Number . hex2dig) x

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              (return . Number . oct2dig) x

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many (oneOf "01")
              (return . Number . bin2dig) x

parseFloat :: Parser LispVal
parseFloat = do 
    x <- many1 digit
    char '.'
    y <- many1 digit
    (return . Float . fst . head . readFloat) (x++"."++y)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> try parseNumber -- 'try' is needed since they can all start with '#'
         <|> try parseBool
         <|> try parseChar

-- Omitted ex 7 



