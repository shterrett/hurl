{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Hurl.ParserSpec (spec) where

import Data.Aeson.QQ (aesonQQ)
import Data.List.NonEmpty (toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Hurl.Parser
import Test.Hspec
import Text.Megaparsec

-- | Run the parser against a Text input
parse' :: Text -> Either (ParseErrorBundle Text HurlParseError) Request
parse' = runParser parseRequest ""

-- | Extract the method value
method :: Request -> Method
method = locValue . requestMethod

-- | Extract the URL value
url :: Request -> Text
url = locValue . requestUrl

-- | Extract the body value
body :: Request -> Maybe Body
body = fmap locValue . requestBody

-- | Extract the option values
options :: Request -> [RequestOption]
options = map locValue . requestOptions

-- | Extract key text from a Key
keyText :: Key -> Text
keyText (BareKey t)   = t
keyText (QuotedKey t) = t

-- | Simplify a Located KeyValue to (key, value)
kvPair :: Located KeyValue -> (Text, Text)
kvPair (Located _ (KeyValue k v)) = (keyText (locValue k), locValue v)

-- | Check if a ParseErrorBundle contains a specific custom error
hasCustomError :: HurlParseError -> ParseErrorBundle Text HurlParseError -> Bool
hasCustomError target bundle =
  any (Set.member (ErrorCustom target) . errorFancies) (toList (bundleErrors bundle))
  where
    errorFancies :: ParseError Text HurlParseError -> Set (ErrorFancy HurlParseError)
    errorFancies (FancyError _ s) = s
    errorFancies _                = Set.empty

spec :: Spec
spec = describe "Hurl.Parser" $ do
  describe "parseRequest" $ do

    describe "success cases" $ do

      it "parses simple GET request" $ do
        src <- TIO.readFile "examples/simple-get.hurl"
        let result = parse' src
        result `shouldSatisfy` \case
          Right req -> method req == GET
                    && url req == "https://google.com"
                    && null (options req)
          Left _    -> False

      it "parses GET with query params" $ do
        src <- TIO.readFile "examples/query-params.hurl"
        let result = parse' src
        case result of
          Left err -> expectationFailure (errorBundlePretty err)
          Right req -> do
            method req `shouldBe` GET
            url req `shouldBe` "https://www.imdb.com"
            case options req of
              [QueryParams kvs] ->
                map kvPair kvs `shouldBe` [("s", "all"), ("q", "star trek")]
              _ -> expectationFailure "expected QueryParams option"

      it "parses POST with JSON body" $ do
        src <- TIO.readFile "examples/post.hurl"
        let result = parse' src
        case result of
          Left err -> expectationFailure (errorBundlePretty err)
          Right req -> do
            method req `shouldBe` POST
            url req `shouldBe` "https://api.example.com/users/123"
            case body req of
              Just (JsonBody (Located _ v)) ->
                v `shouldBe` [aesonQQ| {"email": "john.updated@example.com"} |]
              _ -> expectationFailure "expected JsonBody"

      it "parses POST with form body" $ do
        src <- TIO.readFile "examples/post-form.hurl"
        let result = parse' src
        case result of
          Left err -> expectationFailure (errorBundlePretty err)
          Right req -> do
            method req `shouldBe` POST
            url req `shouldBe` "https://api.example.com/users/123"
            case body req of
              Just (FormBody kvs) ->
                map kvPair kvs `shouldBe` [("email", "john.updated@example.com")]
              _ -> expectationFailure "expected FormBody"

      it "parses GET with insecure option" $ do
        src <- TIO.readFile "examples/insecure.hurl"
        let result = parse' src
        case result of
          Left err -> expectationFailure (errorBundlePretty err)
          Right req -> do
            method req `shouldBe` GET
            url req `shouldBe` "https://google.com"
            options req `shouldBe` [Insecure]

    describe "failure cases" $ do

      it "rejects empty input" $
        parse' "" `shouldSatisfy` isLeft

      it "rejects whitespace-only input" $
        parse' "   \n  " `shouldSatisfy` isLeft

      it "rejects unknown HTTP method" $ do
        let result = parse' "FETCH https://example.com"
        result `shouldSatisfy` isLeft
        case result of
          Left bundle -> bundle `shouldSatisfy` hasCustomError (UnknownMethod "FETCH")
          Right _     -> expectationFailure "expected parse failure"

      it "rejects lowercase method" $
        parse' "get https://example.com" `shouldSatisfy` isLeft

      it "rejects missing URL (EOF after method)" $
        parse' "GET" `shouldSatisfy` isLeft

      it "rejects invalid JSON body" $ do
        let input = "POST https://x.com {\n  json {\n    { invalid json }\n  }\n}"
        parse' input `shouldSatisfy` isLeft

      it "rejects unclosed outer option block" $
        parse' "GET https://x.com {" `shouldSatisfy` isLeft

      it "rejects unknown option keyword" $
        parse' "GET https://x.com { unknown }" `shouldSatisfy` isLeft

      it "rejects missing colon in query block" $
        parse' "GET https://x.com {\n  query {\n    key value\n  }\n}" `shouldSatisfy` isLeft

      it "rejects trailing content after valid request" $
        parse' "GET https://google.com extra" `shouldSatisfy` isLeft

      it "rejects request with both json and form body" $
        parse' "POST https://x.com {\n  json {\n    {\"a\":1}\n  }\n  form {\n    key: \"val\"\n  }\n}"
          `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False
