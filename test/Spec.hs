module Main where

import Test.Hspec
import qualified Hurl.HttpSpec
import qualified Hurl.ParserSpec

main :: IO ()
main = hspec $ do
  Hurl.ParserSpec.spec
  Hurl.HttpSpec.spec
