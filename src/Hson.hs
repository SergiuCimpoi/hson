module Hson (
    parseJsonValue,
)
where

import Control.Applicative
import Core (JsonValue (..), Parser (Parser), parseChar, parseString)
import Number (parseNumber)

-- <JSON_VALUE>       ::= <JSON_NULL>
--                     |  <JSON_BOOL>
--                     |  <JSON_STRING>
--                     |  <NUMBER>
--                     |  <JSON_ARRAY>
--                     |  <JSON_OBJECT>
--
-- <JSON_NULL>        ::= "null"
--
-- <JSON_BOOL>        ::= "true"
--                     |  "false"
--
-- <JSON_STRING>      ::= '"' <CHARACTERS> '"'
--
-- <CHARACTERS>       ::= /* any allowed string content, including escapes */
--
--
-- <JSON_ARRAY>       ::= "[" <ELEMENTS> "]"
--                     |  "[" "]"
--
-- <ELEMENTS>         ::= <JSON_VALUE>
--                     |  <JSON_VALUE> "," <ELEMENTS>
--
-- <JSON_OBJECT>      ::= "{" <MEMBERS> "}"
--                     |  "{" "}"
--
-- <MEMBERS>          ::= <PAIR>
--                     |  <PAIR> "," <MEMBERS>
--
-- <PAIR>             ::= <JSON_STRING> ":" <JSON_VALUE>

parseJsonNull :: Parser JsonValue
parseJsonNull = JsonNull <$ parseString "null"

parseJsonBool :: Parser JsonValue
parseJsonBool =
    (JsonBool True <$ parseString "true")
        <|> (JsonBool False <$ parseString "false")

parseStringLiteral :: Parser String
parseStringLiteral = Parser $ \s -> let (result, rest) = span (/= '"') s in Right (result, rest)

parseJsonString :: Parser JsonValue
parseJsonString = parseChar '"' *> (JsonString <$> parseStringLiteral) <* parseChar '"'

parseJsonNumber :: Parser JsonValue
parseJsonNumber = parseNumber >>= \input -> return $ JsonNumber $ read input

parseJsonValue :: Parser JsonValue
parseJsonValue = parseJsonNull <|> parseJsonBool <|> parseJsonString <|> parseJsonNumber

-- eatBlanks :: String -> String
-- eatBlanks str@(x : xs)
--     | x == ' ' = eatBlanks xs
--     | otherwise = str
-- eatBlanks [] = []
