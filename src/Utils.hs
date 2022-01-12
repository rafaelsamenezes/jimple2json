module Utils where

import ToJson ()
import Parser ( parseTopLevel )
import Data.Aeson ( encode )

process line = do
    let res = parseTopLevel line
    case res of
      Left err -> Nothing
      Right program -> Just $ encode program
