module Main where

import Parser
import Control.Monad.Trans
import System.Console.Haskeline
import System.Environment
import System.IO

import ToJson
import Data.Aeson
import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString.Lazy.Char8 as BL

process :: String -> String -> IO ()
process line dest = do
    let res = parseTopLevel line
    case res of
      Left err -> print err
      Right program -> B.writeFile dest $ encode program
      --Right program -> print $ encode program

processArg :: FilePath -> IO ()
processArg x = do
  content <- readFile x
  process content (x ++ ".ast")

main :: IO()
main = do
  args <- getArgs
  mapM_ processArg args


-- main :: IO ()
-- main = runInputT defaultSettings loop
--   where
--   loop = do
--     minput <- getInputLine "ready> "
--     case minput of
--       Nothing -> outputStrLn "Goodbye."
--       Just x  -> (liftIO $ process x) >> loop