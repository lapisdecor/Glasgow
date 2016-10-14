import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.List ( intercalate )


data Tag = MkTag String deriving Show

parseTag :: Parser Tag
parseTag =
  do  char '<'
      x <- identifier
      char '>'
      return (MkTag x)
-- usage: parseTest parseTag "<div>"

parseDiv :: Parser Tag
parseDiv = do 
  string "<div>" 
  return (MkTag "div")
-- usage: parseTest parseDiv "<div>"


letter_digit :: Parser Char
letter_digit =
  do  x <- letter <|> digit
      return x
-- usage: parseTest letter_digit "2a"

bag_bog_try :: Parser String
bag_bog_try =
  do  xs <- try (string "bag") <|> string "bog"
      return xs
-- usage: parseTest bag_bog_try "bog"

varname :: Parser String
varname =
  do  x <- letter
      xs <- many (letter <|> digit)
      return (x:xs)
-- usage: parseTest varname "a728b*3"
-- result "a728b"





lexer       = P.makeTokenParser emptyDef

parens          = P.parens lexer    
brackets        = P.brackets lexer    
braces          = P.braces lexer    
commaSep        = P.commaSep lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer
integer         = P.integer lexer    
stringLiteral = P.stringLiteral lexer
