{-# LANGUAGE OverloadedStrings #-}

module Hurl.Parser
  ( Request(..), Method(..), RequestOption(..), KeyValue(..), Key(..)
  , Span(..), Located(..), HurlParseError(..), Parser
  , parseRequest
  ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Char (isAlphaNum, isSpace, isUpper)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Error.Diagnose (Note(..))
import Error.Diagnose.Compat.Megaparsec (HasHints(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Source location
data Span = Span { spanStart :: SourcePos, spanEnd :: SourcePos }
  deriving (Show, Eq)

data Located a = Located { locSpan :: Span, locValue :: a }
  deriving (Show, Eq, Functor)

-- HTTP Method
data Method = GET | POST | PUT | PATCH | DELETE | HEAD | OPTIONS
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Key-value pair keys: bare identifiers or quoted strings
data Key = BareKey Text | QuotedKey Text
  deriving (Show, Eq)

data KeyValue = KeyValue { kvKey :: Located Key, kvValue :: Located Text }
  deriving (Show, Eq)

-- Request options (the contents of the outer { } block)
data RequestOption
  = QueryParams [Located KeyValue]
  | JsonBody    (Located Value)
  | FormBody    [Located KeyValue]
  | Insecure
  deriving (Show, Eq)

-- Top-level AST node
data Request = Request
  { requestMethod  :: Located Method
  , requestUrl     :: Located Text
  , requestOptions :: [Located RequestOption]
  } deriving (Show, Eq)

-- Custom parse error
data HurlParseError
  = UnknownMethod Text
  | InvalidJson   Text
  | UnterminatedJson
  deriving (Show, Eq, Ord)

type Parser = Parsec HurlParseError Text

instance ShowErrorComponent HurlParseError where
  showErrorComponent (UnknownMethod m) = "unknown HTTP method: " <> Text.unpack m
  showErrorComponent (InvalidJson msg) = "invalid JSON body: " <> Text.unpack msg
  showErrorComponent UnterminatedJson  = "unterminated JSON body (unmatched '{')"

instance HasHints HurlParseError Text where
  hints (UnknownMethod _) = []
  hints (InvalidJson _)   = [Note "the JSON body must be a valid JSON value"]
  hints UnterminatedJson  = [Note "check that every '{' has a matching '}'"]

-- Infrastructure

-- space consumer: spaces/tabs/newlines, no comment syntax
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- annotate with start+end source position
located :: Parser a -> Parser (Located a)
located p = do
  start <- getSourcePos
  val   <- p
  end   <- getSourcePos
  pure (Located (Span start end) val)

-- Atomic parsers

pMethod :: Parser (Located Method)
pMethod = located $ lexeme $ do
  w <- takeWhile1P (Just "HTTP method") isUpper
  case w of
    "GET"     -> pure GET
    "POST"    -> pure POST
    "PUT"     -> pure PUT
    "PATCH"   -> pure PATCH
    "DELETE"  -> pure DELETE
    "HEAD"    -> pure HEAD
    "OPTIONS" -> pure OPTIONS
    other     -> customFailure (UnknownMethod other)

pUrl :: Parser (Located Text)
pUrl = located $ lexeme $ takeWhile1P (Just "URL") (not . isSpace)

pStringLiteral :: Parser Text
pStringLiteral = do
  _ <- char '"'
  cs <- manyTill L.charLiteral (char '"')
  pure (Text.pack cs)

pKey :: Parser (Located Key)
pKey = located $ lexeme $
      (QuotedKey <$> pStringLiteral)
  <|> (BareKey  <$> takeWhile1P (Just "key") (\c -> isAlphaNum c || c == '-' || c == '_'))

pKeyValue :: Parser (Located KeyValue)
pKeyValue = located $ do
  k <- pKey
  _ <- symbol ":"
  v <- located (lexeme pStringLiteral)
  pure (KeyValue k v)

-- Option parsers

pQueryBlock :: Parser (Located RequestOption)
pQueryBlock = located $ do
  _ <- symbol "query"
  kvs <- between (symbol "{") (symbol "}") (many pKeyValue)
  pure (QueryParams kvs)

pFormBlock :: Parser (Located RequestOption)
pFormBlock = located $ do
  _ <- symbol "form"
  kvs <- between (symbol "{") (symbol "}") (many pKeyValue)
  pure (FormBody kvs)

-- Capture a balanced { ... } as Text, then decode with Aeson
pBalancedBraces :: Parser Text
pBalancedBraces = do
  sc
  _ <- char '{'
  go 1 "{"
  where
    go :: Int -> Text -> Parser Text
    go 0 acc = pure acc
    go d acc = do
      c <- anySingle
      let d' = case c of '{' -> d + 1; '}' -> d - 1; _ -> d
      go d' (acc <> Text.singleton c)

pJsonBlock :: Parser (Located RequestOption)
pJsonBlock = located $ do
  _ <- symbol "json"
  _ <- symbol "{"                          -- outer option-block brace
  raw <- lexeme pBalancedBraces            -- inner { ... } JSON literal + trailing ws
  _ <- symbol "}"                          -- close outer option-block brace
  start <- getSourcePos
  end   <- getSourcePos
  case Aeson.eitherDecodeStrict (Text.encodeUtf8 raw) of
    Left err -> customFailure (InvalidJson (Text.pack err))
    Right v  -> pure (JsonBody (Located (Span start end) v))

pInsecure :: Parser (Located RequestOption)
pInsecure = located (symbol "insecure" $> Insecure)

pOption :: Parser (Located RequestOption)
pOption =
      try pQueryBlock
  <|> try pJsonBlock
  <|> try pFormBlock
  <|> pInsecure
  <?> "request option (query, json, form, or insecure)"

pOptionBlock :: Parser [Located RequestOption]
pOptionBlock = between (symbol "{") (symbol "}") (many pOption)

-- Top-level entry point
parseRequest :: Parser Request
parseRequest = do
  sc
  m    <- pMethod
  u    <- pUrl
  opts <- fromMaybe [] <$> optional pOptionBlock
  sc
  eof
  pure (Request m u opts)
