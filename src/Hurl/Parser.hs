{-# LANGUAGE OverloadedStrings #-}

module Hurl.Parser
  ( Request(..), Method(..), RequestOption(..), KeyValue(..), Key(..), Body(..)
  , Span(..), Located(..), HurlParseError(..), Parser
  , keyText
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

-- Body variants for a request
data Body
  = JsonBody (Located Value)
  | FormBody [Located KeyValue]
  deriving (Show, Eq)

-- Request options (the contents of the outer { } block, excluding body and query params)
data RequestOption
  = Insecure
  deriving (Show, Eq)

-- Top-level AST node
data Request = Request
  { requestMethod      :: Located Method
  , requestUrl         :: Located Text        -- base URL only, no "?..." part
  , requestBody        :: Maybe (Located Body)
  , requestQueryParams :: [Located KeyValue]  -- merged (URL params ++ block params, block wins)
  , requestOptions     :: [Located RequestOption]
  } deriving (Show, Eq)

-- Custom parse error
data HurlParseError
  = UnknownMethod Text
  | InvalidJson   Text
  | UnterminatedJson
  | DuplicateBody
  deriving (Show, Eq, Ord)

type Parser = Parsec HurlParseError Text

instance ShowErrorComponent HurlParseError where
  showErrorComponent (UnknownMethod m) = "unknown HTTP method: " <> Text.unpack m
  showErrorComponent (InvalidJson msg) = "invalid JSON body: " <> Text.unpack msg
  showErrorComponent UnterminatedJson  = "unterminated JSON body (unmatched '{')"
  showErrorComponent DuplicateBody     = "a request may have at most one body (json or form)"

instance HasHints HurlParseError Text where
  hints (UnknownMethod _) = []
  hints (InvalidJson _)   = [Note "the JSON body must be a valid JSON value"]
  hints UnterminatedJson  = [Note "check that every '{' has a matching '}'"]
  hints DuplicateBody     = []

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

-- | Extract the text of a Key
keyText :: Key -> Text
keyText (BareKey t)   = t
keyText (QuotedKey t) = t

-- URL parsers

-- Parses a single key=value pair from a URL query string (no quotes, no colon)
pQueryStringParam :: Parser (Located KeyValue)
pQueryStringParam = located $ do
  k <- located $ BareKey <$> takeWhile1P (Just "query param key")
                                (\c -> c /= '=' && c /= '&' && not (isSpace c))
  _ <- char '='
  v <- located $ takeWhileP (Just "query param value")
                              (\c -> c /= '&' && not (isSpace c))
  pure (KeyValue k v)

-- Replaces pUrl; returns (base URL text, query-string params)
pUrlAndParams :: Parser (Located Text, [Located KeyValue])
pUrlAndParams = do
  base   <- located $ takeWhile1P (Just "URL") (\c -> not (isSpace c) && c /= '?')
  params <- option [] (char '?' *> sepBy pQueryStringParam (char '&'))
  sc
  pure (base, params)

-- Option parsers

pQueryBlock :: Parser [Located KeyValue]
pQueryBlock = do
  _ <- symbol "query"
  between (symbol "{") (symbol "}") (many pKeyValue)

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

pJsonBody :: Parser (Located Body)
pJsonBody = located $ do
  _ <- symbol "json"
  start <- getSourcePos
  raw   <- lexeme pBalancedBraces          -- { ... } is the JSON object itself
  end   <- getSourcePos
  case Aeson.eitherDecodeStrict (Text.encodeUtf8 raw) of
    Left err -> customFailure (InvalidJson (Text.pack err))
    Right v  -> pure (JsonBody (Located (Span start end) v))

pFormBody :: Parser (Located Body)
pFormBody = located $ do
  _ <- symbol "form"
  kvs <- between (symbol "{") (symbol "}") (many pKeyValue)
  pure (FormBody kvs)

pInsecure :: Parser (Located RequestOption)
pInsecure = located (symbol "insecure" $> Insecure)

data BlockItem
  = BodyItem  (Located Body)
  | QueryItem [Located KeyValue]
  | OptionItem (Located RequestOption)

pBlockItem :: Parser BlockItem
pBlockItem =
      (BodyItem   <$> try pJsonBody)
  <|> (BodyItem   <$> try pFormBody)
  <|> (QueryItem  <$> try pQueryBlock)
  <|> (OptionItem <$> pInsecure)
  <?> "request option (query, json, form, or insecure)"

pOptionBlock :: Parser (Maybe (Located Body), [Located KeyValue], [Located RequestOption])
pOptionBlock = between (symbol "{") (symbol "}") $ do
  items <- many pBlockItem
  let bodies  = [b | BodyItem  b <- items]
      queries = concat [q | QueryItem  q <- items]
      opts    = [o | OptionItem o <- items]
  case bodies of
    []  -> pure (Nothing, queries, opts)
    [b] -> pure (Just b,  queries, opts)
    _   -> customFailure DuplicateBody

-- | Merge URL query params and block query params; block params take precedence.
mergeQueryParams :: [Located KeyValue] -> [Located KeyValue] -> [Located KeyValue]
mergeQueryParams urlParams blockParams =
  let blockKeys = map (keyText . locValue . kvKey . locValue) blockParams
      urlOnly   = filter (\kv -> keyText (locValue (kvKey (locValue kv))) `notElem` blockKeys) urlParams
  in urlOnly <> blockParams

-- Top-level entry point
parseRequest :: Parser Request
parseRequest = do
  sc
  m <- pMethod
  (u, urlParams) <- pUrlAndParams
  (mbody, blockParams, opts) <- fromMaybe (Nothing, [], []) <$> optional pOptionBlock
  sc
  eof
  pure (Request m u mbody (mergeQueryParams urlParams blockParams) opts)
