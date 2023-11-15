{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hurl.Parser where

import Language.ANTLR4
import Data.Text (Text)
import Data.Aeson qualified as A
import Network.HTTP.Types.Method (StdMethod(..), parseMethod)
import Data.ByteString (ByteString)

data ParsedRequest = ParsedRequest
  { url :: Text
  , headers :: [(ByteString, ByteString)]
  , verb :: Text
  , body :: ByteString
  }
  deriving (Show, Generic)

[g4|
grammar HttpRequest;

request: 'request' '{' requestParams '}' ;

requestParams: urlParam headersParam bodyParam verbParam ;

urlParam: 'url:' STRING ;

headersParam: 'headers:' '[' headerList ']' ;

headerList: header (',' header)* ;

header: STRING ':' STRING ;

bodyParam: 'body:' JSON ;

verbParam: 'verb:' STRING ;

JSON: '{' .*? '}' ;

STRING: '"' ~("}" | "\"" | "\r" | "\n")* '"';

WS: [ \t\r\n]+ -> skip;
|]
