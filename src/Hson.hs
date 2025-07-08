module Hson (parseJsonValue, parsePair, parseMembers)
where

import Control.Applicative
import Core (JsonValue (..), Parser (Parser), eatBlanks, parseChar, parseString)
import Data.Functor (($>))
import Number (parseNumber)

-- <JSON_NULL>        ::= "null"
parseJsonNull :: Parser JsonValue
parseJsonNull = JsonNull <$ parseString "null"

-- <JSON_BOOL>        ::= "true" | "false"
parseJsonBool :: Parser JsonValue
parseJsonBool = (JsonBool True <$ parseString "true") <|> (JsonBool False <$ parseString "false")

-- <STRING_LITERAL>       ::= /* any allowed string content, including escapes */
parseStringLiteral :: Parser String
parseStringLiteral = Parser $ \s -> let (result, rest) = span (/= '"') s in Right (result, rest)

-- <JSON_STRING>      ::= '"' <STRING_LITERAL> '"'
parseJsonString :: Parser JsonValue
parseJsonString = parseChar '"' *> (JsonString <$> parseStringLiteral) <* parseChar '"'

-- the heavy lifting is done by parseNumber in Core module
parseJsonNumber :: Parser JsonValue
parseJsonNumber = parseNumber >>= \number -> return $ JsonNumber $ read number

-- <ELEMENTS>         ::= <JSON_VALUE> | <JSON_VALUE> "," <ELEMENTS>
parseElements :: Parser [JsonValue]
parseElements =
    ( do
        v <- parseJsonValue
        _ <- eatBlanks
        _ <- parseChar ','
        _ <- eatBlanks
        vs <- parseElements
        return (v : vs)
    )
        <|> pure <$> parseJsonValue

-- <JSON_ARRAY>       ::= "[" <ELEMENTS> "]" | "[" "]"
parseJsonArray :: Parser JsonValue
parseJsonArray = parseChar '[' *> eatBlanks *> (JsonArray <$> parseElements) <* eatBlanks <* parseChar ']'

-- <PAIR>             ::= <JSON_STRING> ":" <JSON_VALUE>
parsePair :: Parser (JsonValue, JsonValue)
parsePair = do
    key <- parseJsonString
    _ <- eatBlanks
    _ <- parseChar ':'
    _ <- eatBlanks
    value <- parseJsonValue
    return (key, value)

-- <MEMBERS>          ::= <PAIR> | <PAIR> "," <MEMBERS>
parseMembers :: Parser [(JsonValue, JsonValue)]
parseMembers =
    ( do
        pair <- parsePair
        _ <- eatBlanks
        _ <- parseChar ','
        _ <- eatBlanks
        pairs <- parseMembers
        return (pair : pairs)
    )
        <|> pure <$> parsePair

-- <JSON_OBJECT>      ::= "{" <MEMBERS> "}" | "{" "}"
parseJsonObject :: Parser JsonValue
parseJsonObject =
    ( do
        _ <- parseChar '{'
        _ <- eatBlanks
        members <- parseMembers
        _ <- eatBlanks
        _ <- parseChar '}'
        return $ JsonObject members
    )
        <|> JsonObject <$> ((parseChar '{' *> eatBlanks *> parseChar '}') $> [])

-- <JSON_VALUE>       ::= <JSON_NULL>
--                     |  <JSON_BOOL>
--                     |  <JSON_STRING>
--                     |  <NUMBER>
--                     |  <JSON_ARRAY>
--                     |  <JSON_OBJECT>
parseJsonValue :: Parser JsonValue
parseJsonValue =
    asum
        [ parseJsonNull
        , parseJsonBool
        , parseJsonString
        , parseJsonNumber
        , parseJsonArray
        , parseJsonObject
        ]
