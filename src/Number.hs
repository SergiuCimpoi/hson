{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Number (
    parseNumber,
)
where

import Control.Applicative ((<|>))
import Core (Parser (Parser), parseChar)
import Data.Char (isDigit)

-- <SIGN> ::= "" | "-"
parseSign :: Parser String
parseSign = pure <$> parseChar '-' <|> pure ""

-- <ONENINE> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
parseOneNine :: Parser String
parseOneNine = Parser $ \input -> case input of
    (c : cs)
        | c >= '1' && c <= '9' -> Right (pure c, cs)
        | otherwise -> Left ["expected a digit 1..9, got " ++ take 10 (c : cs)]
    "" -> Left ["empty input while parsing a number"]

-- <DIGIT> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
parseDigit :: Parser Char
parseDigit = Parser $ \input -> case input of
    (c : cs)
        | isDigit c -> Right (c, cs)
        | otherwise -> Left ["invalid digit"]
    "" -> Left ["empty string"]

-- <DIGITS> ::= "" | <DIGIT> <DIGITS>
parseDigits :: Parser String
parseDigits =
    ( do
        d <- parseDigit
        ds <- parseDigits
        return (d : ds)
    )
        <|> pure ""

-- <INT> ::= "0" | <ONENINE> <DIGITS>
parseInt :: Parser String
parseInt = pure <$> parseChar '0' <|> ((++) <$> parseOneNine <*> parseDigits)

-- <DIGITS_NONZERO> ::= <DIGIT> | <DIGIT> <DIGITS_NONZERO>
parseDigitsNonZero :: Parser String
parseDigitsNonZero =
    ( do
        d <- parseDigit
        ds <- parseDigitsNonZero
        return (d : ds)
    )
        <|> pure <$> parseDigit

-- <FRAC> ::= "" | "." <DIGITS_NONZERO>
parseFrac :: Parser String
parseFrac =
    ( do
        d <- parseChar '.'
        ds <- parseDigitsNonZero
        return (d : ds)
    )
        <|> pure ""

-- <SIGN_EXP> ::= "" | "+" | "-"
parseSignExp :: Parser String
parseSignExp =
    pure <$> (parseChar '+' <|> parseChar '-')
        <|> pure ""

-- <E> ::= "e" | "E"
-- <EXP> ::= "" | <E> <SIGN_EXP> <DIGITS_NONZERO>
parseExp :: Parser String
parseExp =
    ( do
        e <- parseChar 'e' <|> parseChar 'E'
        s <- parseSignExp
        ds <- parseDigitsNonZero
        return (e : s ++ ds)
    )
        <|> pure ""

-- <NUMBER> ::= <SIGN> <INT> <FRAC> <EXP>
parseNumber :: Parser String
parseNumber = do
    s <- parseSign
    i <- parseInt
    f <- parseFrac
    e <- parseExp
    return (concat [s, i, f, e])
