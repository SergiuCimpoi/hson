{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Core (JsonValue (..), Parser (runParser, Parser), parseChar, parseString, satisfy, eatBlanks)
where

import Control.Applicative
import qualified Data.Bifunctor
import Data.Char (isSpace)
import Data.Functor (void)

data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonString String
    | JsonNumber Double
    | JsonArray [JsonValue]
    | JsonObject [(JsonValue, JsonValue)]
    deriving (Show)

newtype Parser a = Parser {runParser :: String -> Either String (a, String)}

instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap (Data.Bifunctor.first f) . p

instance Applicative Parser where
    pure a = Parser $ \input -> Right (a, input)
    (Parser pf) <*> (Parser p) = Parser $ \input -> do
        (f, input1) <- pf input
        (result, input2) <- p input1
        return (f result, input2)

instance Monad Parser where
    (Parser p) >>= f2 = Parser $ \input -> do
        (result, input1) <- p input
        runParser (f2 result) input1

instance Alternative Parser where
    empty = Parser $ \_ -> Left ""
    (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
        Right r -> Right r
        _ -> p2 input

parseChar :: Char -> Parser Char
parseChar ch = Parser p
  where
    p "" = Left "error empty string"
    p (c : cs)
        | ch == c = Right (ch, cs)
        | otherwise = Left $ "error: expected " ++ [ch] ++ ", got " ++ [c]

parseString :: String -> Parser String
parseString = traverse parseChar

satisfy :: (Char -> Bool) -> Parser Char
satisfy fn = Parser $ \input -> case input of
    "" -> Left ""
    (c : cs)
        | fn c -> Right (c, cs)
        | otherwise -> Left ""

eatBlanks :: Parser ()
eatBlanks = void $ many (satisfy isSpace)
