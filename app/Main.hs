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

import Game.State ( GameState )
import Game.Logic (takeTurn)

main :: IO()
main = do
  hSetBuffering stdout LineBuffering

  B.interact $ unlines . map handleInput . splitIntoInputs

handleInput :: ByteString -> ByteString
handleInput = JSON.encode . handleState . JSON.eitherDecode
  where
    handleState :: Either String GameState -> GameState
    handleState (Right state) = takeTurn state
    handleState (Left err) = error err


unlines :: [ByteString] -> ByteString
unlines = B.intercalate "\n"

-- }{ --> ["}","{"]
splitIntoInputs :: ByteString -> [ByteString]
splitIntoInputs = B.split 10
