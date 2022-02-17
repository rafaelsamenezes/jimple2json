module Utils where

import ToJson ()
import Parser.Parser ( parseTopLevel )
import Data.Aeson ( encode )

process line = do
    let res = parseTopLevel line
    case res of
      Left err -> Left err
      Right program -> Right $ encode program

