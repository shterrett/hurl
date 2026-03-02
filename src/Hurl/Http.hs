{-# LANGUAGE OverloadedStrings #-}

module Hurl.Http
  ( HurlResponse(..)
  , HttpError(..)
  , sendRequest
  ) where

import Control.Exception (catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextE
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManager, newTlsManagerWith)
import Network.Connection (TLSSettings(..))
import qualified Network.TLS as TLS
import Network.HTTP.Types (hContentType, renderSimpleQuery, statusCode)
import qualified Hurl.Parser as P

data HurlResponse = HurlResponse
  { hurlResponseStatus  :: Int
  , hurlResponseHeaders :: [(BS.ByteString, BS.ByteString)]
  , hurlResponseBody    :: LBS.ByteString
  } deriving (Show)

newtype HttpError = HttpError HTTP.HttpException
  deriving (Show)

sendRequest :: P.Request -> IO (Either HttpError HurlResponse)
sendRequest hurlReq = do
  manager <- makeManager (P.requestOptions hurlReq)
  let urlStr = Text.unpack (P.locValue (P.requestUrl hurlReq))
  fmap (Right . fromResponse) (runRequest urlStr manager)
    `catch` (pure . Left . HttpError)
  where
    runRequest urlStr manager = do
      base <- HTTP.parseRequest urlStr
      HTTP.httpLbs (applyOptions hurlReq base) manager
    fromResponse r = HurlResponse
      { hurlResponseStatus  = statusCode (HTTP.responseStatus r)
      , hurlResponseHeaders = map (\(k, v) -> (CI.original k, v)) (HTTP.responseHeaders r)
      , hurlResponseBody    = HTTP.responseBody r
      }

makeManager :: [P.Located P.RequestOption] -> IO HTTP.Manager
makeManager opts
  | any ((== P.Insecure) . P.locValue) opts =
      newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False True TLS.defaultSupported) Nothing)
  | otherwise = newTlsManager

methodToBS :: P.Method -> BS.ByteString
methodToBS P.GET     = "GET"
methodToBS P.POST    = "POST"
methodToBS P.PUT     = "PUT"
methodToBS P.PATCH   = "PATCH"
methodToBS P.DELETE  = "DELETE"
methodToBS P.HEAD    = "HEAD"
methodToBS P.OPTIONS = "OPTIONS"

applyOptions :: P.Request -> HTTP.Request -> HTTP.Request
applyOptions hurlReq base =
  let withMethod = base { HTTP.method = methodToBS (P.locValue (P.requestMethod hurlReq)) }
      withParams = setQueryParams (P.requestQueryParams hurlReq) withMethod
  in case P.requestBody hurlReq of
       Nothing      -> withParams
       Just locBody -> setBody (P.locValue locBody) withParams

setBody :: P.Body -> HTTP.Request -> HTTP.Request
setBody (P.JsonBody lv) req = req
  { HTTP.requestBody    = HTTP.RequestBodyLBS (Aeson.encode (P.locValue lv))
  , HTTP.requestHeaders = (hContentType, "application/json")
                          : HTTP.requestHeaders req
  }
setBody (P.FormBody kvs) req = req
  { HTTP.requestBody    = HTTP.RequestBodyBS (renderSimpleQuery False params)
  , HTTP.requestHeaders = (hContentType, "application/x-www-form-urlencoded")
                          : HTTP.requestHeaders req
  }
  where
    params = map kvToBS kvs
    kvToBS lkv =
      let kv = P.locValue lkv
      in ( TextE.encodeUtf8 (P.keyText (P.locValue (P.kvKey kv)))
         , TextE.encodeUtf8 (P.locValue (P.kvValue kv))
         )

setQueryParams :: [P.Located P.KeyValue] -> HTTP.Request -> HTTP.Request
setQueryParams kvs req = HTTP.setQueryString params req
  where
    params = map kvToParam kvs
    kvToParam lkv =
      let kv = P.locValue lkv
      in ( TextE.encodeUtf8 (P.keyText (P.locValue (P.kvKey kv)))
         , Just (TextE.encodeUtf8 (P.locValue (P.kvValue kv)))
         )
