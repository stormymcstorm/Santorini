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

import Game.State ( State )
import Logic.State (takeTurn)
import JSON.State ()
-- import Debug.Trace (trace)

main :: IO()
main = do
  hSetBuffering stdout LineBuffering

  B.interact $ unlines . map handleInput . splitIntoInputs

handleInput :: ByteString -> ByteString
handleInput = JSON.encode . handleState . JSON.eitherDecode
  where
    handleState :: Either String State -> State
    -- handleState (Right state) = (\s -> trace (show s) s) . takeTurn $ state
    handleState (Right state) = takeTurn state
    handleState (Left err) = error err


unlines :: [ByteString] -> ByteString
unlines = B.intercalate "\n"

splitIntoInputs :: ByteString -> [ByteString]
splitIntoInputs = B.split 10
