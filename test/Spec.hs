module Main where

import Test.Hspec
import qualified Hurl.ParserSpec

main :: IO ()
main = hspec $ do
  Hurl.ParserSpec.spec
