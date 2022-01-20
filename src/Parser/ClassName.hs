module Parser.ClassName (jimpleClassName) where

import Text.Parsec.String (Parser)
import Ast ( ClassName(..) ) 
import Lexer ( identifier, fullIdentifier )
import Text.Parsec ( (<|>), try ) 

jimpleIdentifier :: Parser ClassName
jimpleIdentifier = Identifier <$> identifier

jimpleFullIdentifier :: Parser ClassName
jimpleFullIdentifier = FullIdentifier <$> fullIdentifier

jimpleClassName :: Parser ClassName
jimpleClassName =
  try jimpleFullIdentifier
    <|> try jimpleIdentifier