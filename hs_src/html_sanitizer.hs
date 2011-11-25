module Main where

import Text.HTML.SanitizeXSS
import System.IO
import Data.Text (pack, unpack)

import Data.Binary.Get
import Data.Binary.Put
import Data.Binary
import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BU
import Control.Monad

main = do
  i <- B.hGet stdin 4
  let len = runGet getWord32be i
  content <- B.hGet stdin (fromIntegral len)
  let sanitized = BU.fromString . unpack . sanitizeBalance . pack $ BU.toString content
  let result = runPut $ putWord32be (fromIntegral $ B.length sanitized)
  B.hPut stdout result
  B.hPut stdout sanitized
  hClose stdin
  hClose stdout
