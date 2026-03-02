{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hurl.HttpSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Text as Text
import Hurl.Http
import qualified Hurl.Parser as P
import Network.HTTP.Types (status200)
import Network.Wai (Application, rawQueryString, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import Text.Megaparsec (SourcePos(..), mkPos)

-- | Dummy source position for constructing test AST nodes
dummyPos :: SourcePos
dummyPos = SourcePos "" (mkPos 1) (mkPos 1)

dummySpan :: P.Span
dummySpan = P.Span dummyPos dummyPos

loc :: a -> P.Located a
loc = P.Located dummySpan

-- | Build a Located KeyValue from bare key and value texts
mkKV :: Text.Text -> Text.Text -> P.Located P.KeyValue
mkKV k v = loc (P.KeyValue (loc (P.BareKey k)) (loc v))

-- | Construct a test Request
mkRequest :: Text.Text -> P.Method -> Maybe P.Body -> [P.Located P.KeyValue] -> P.Request
mkRequest url meth mbody params = P.Request
  { P.requestMethod      = loc meth
  , P.requestUrl         = loc url
  , P.requestBody        = fmap loc mbody
  , P.requestQueryParams = params
  , P.requestOptions     = []
  }

-- | Simple WAI app that echoes query string + request body
testApp :: Application
testApp req respond = do
  body <- strictRequestBody req
  let qs = rawQueryString req
  respond $ responseLBS status200 [] (LBS.fromStrict qs <> body)

baseUrl :: Int -> Text.Text
baseUrl port = "http://localhost:" <> Text.pack (show port)

spec :: Spec
spec = describe "Hurl.Http" $ around (testWithApplication (pure testApp)) $ do

  it "sends GET and returns 200" $ \port -> do
    let req = mkRequest (baseUrl port) P.GET Nothing []
    result <- sendRequest req
    case result of
      Left e  -> expectationFailure (show e)
      Right r -> hurlResponseStatus r `shouldBe` 200

  it "sends query params" $ \port -> do
    let req = mkRequest (baseUrl port) P.GET Nothing
                [mkKV "foo" "bar", mkKV "baz" "qux"]
    result <- sendRequest req
    case result of
      Left e  -> expectationFailure (show e)
      Right r -> do
        let body = LBS.toStrict (hurlResponseBody r)
        body `shouldSatisfy` ("foo=bar" `BS.isInfixOf`)
        body `shouldSatisfy` ("baz=qux" `BS.isInfixOf`)

  it "sends POST with JSON body" $ \port -> do
    let jsonVal = [aesonQQ| {"name": "alice"} |]
        req = mkRequest (baseUrl port) P.POST
                (Just (P.JsonBody (loc jsonVal))) []
    result <- sendRequest req
    case result of
      Left e  -> expectationFailure (show e)
      Right r -> do
        hurlResponseStatus r `shouldBe` 200
        let body = LBS.toStrict (hurlResponseBody r)
        body `shouldSatisfy` ("alice" `BS.isInfixOf`)

  it "sends POST with form body" $ \port -> do
    let req = mkRequest (baseUrl port) P.POST
                (Just (P.FormBody [mkKV "key" "value", mkKV "name" "alice"])) []
    result <- sendRequest req
    case result of
      Left e  -> expectationFailure (show e)
      Right r -> do
        hurlResponseStatus r `shouldBe` 200
        let body = LBS.toStrict (hurlResponseBody r)
        body `shouldSatisfy` ("key=value" `BS.isInfixOf`)
        body `shouldSatisfy` ("name=alice" `BS.isInfixOf`)
