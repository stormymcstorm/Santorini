{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (lines, unlines, interact, getLine, putStrLn)
import Debug.Trace (trace)
import System.IO (stdout, hSetBuffering, BufferMode(..), hFlush)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
-- import Data.ByteString.Char8 (ByteString, putStrLn, getLine)
import qualified Data.Aeson as JSON

import BoardState

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering 

  BL.interact $ unlines . map handleLine . lines


handleLine :: ByteString -> ByteString
handleLine = JSON.encode . handleState . JSON.decode
  where
    handleState :: Maybe BoardState -> BoardState
    handleState (Just s) =  takeTurn s
    handleState Nothing = error "Failed to parse input"

lines :: ByteString -> [ByteString]
lines =  BL.split 10

unlines :: [ByteString] -> ByteString
unlines = BL.intercalate "\n"