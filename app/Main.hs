{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import Hurl (HurlResponse(..), parseRequest, sendRequest)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn)
import Text.Megaparsec (runParser)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- Text.readFile filename
      case runParser parseRequest filename content of
        Right req -> do
          result <- sendRequest req
          case result of
            Left httpErr -> do
              hPutStrLn stderr ("HTTP error: " <> show httpErr)
              exitFailure
            Right resp -> printResponse resp
        Left bundle -> do
          let diag  = errorDiagnosticFromBundle
                        (Nothing :: Maybe Text)
                        ("Parse error" :: Text)
                        (Nothing :: Maybe [Note Text])
                        bundle
              diag' = addFile diag filename (Text.unpack content)
          printDiagnostic stderr WithUnicode (TabSize 4) defaultStyle diag'
          exitFailure
    _ -> putStrLn "Usage: hurl-exe <filename.hurl>" >> exitFailure

printResponse :: HurlResponse -> IO ()
printResponse resp = do
  putStrLn ("HTTP " <> show (hurlResponseStatus resp))
  mapM_ printHeader (hurlResponseHeaders resp)
  putStrLn ""
  LBS.putStr (hurlResponseBody resp)
  where
    printHeader (name, value) = BSC.putStrLn (name <> ": " <> value)
