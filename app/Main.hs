{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (unlines)

import System.IO (
    stdout
  , hSetBuffering
  , BufferMode(..)
  )
import Data.Word (Word8)

import qualified Data.Aeson as JSON

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy ( ByteString )


main :: IO()
main = do
  hSetBuffering stdout LineBuffering

  -- undefined 
  B.interact $ unlines . map handleInput . splitIntoInputs

handleInput :: ByteString -> ByteString 
handleInput = id

unlines :: [ByteString] -> ByteString 
unlines = B.intercalate "\n" 

splitIntoInputs :: ByteString -> [ByteString]
splitIntoInputs = B.split 10