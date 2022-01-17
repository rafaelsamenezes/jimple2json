module Main where

import qualified Utils as U

import System.Environment ( getArgs )
import System.IO

import qualified Data.ByteString.Lazy as B

generateOutput :: String -> String -> IO ()
generateOutput line dest = do
    let res = U.process line
    case res of
      Nothing -> print "Couldn't parse Jimple File"
      Just x -> B.writeFile dest x

processArg :: FilePath -> IO ()
processArg x = do
  content <- readFile x
  generateOutput content (x ++ ".json")

main :: IO()
main = do
  args <- getArgs
  mapM_ processArg args