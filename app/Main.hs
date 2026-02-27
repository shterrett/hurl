{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import Hurl (parseRequest)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (runParser)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- Text.readFile filename
      case runParser parseRequest filename content of
        Right req -> print req
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
