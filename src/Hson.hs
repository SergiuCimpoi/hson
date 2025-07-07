module Hson (
    parseJsonValue,
)
where

import Control.Applicative
import Core (JsonValue (..), Parser (Parser), parseChar, parseString)
import Number (parseNumber)

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
