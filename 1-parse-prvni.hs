{------------------------------------------------------------------------------

  1-parse-prvni.hs
  -----------------------------------
  parsing experiments for penalize.hs

------------------------------------------------------------------------------}

module PenalParse where

import Char
import Control.Monad
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding ( many, optional, (<|>) )
import Control.Applicative
import Data.Either
import Data.Maybe

import System.Time
import Data.Time


instance Applicative (GenParser a st) where
  (<*>) = ap
  pure = return
  
instance Alternative (GenParser a st) where
  empty = mzero
  (<|>) = mplus

data Client = Cl { clientNo :: String, getClientName :: String,
                   getIntrP :: Float, getPenP :: Float, dailyP :: Bool }
              deriving (Show,Eq)
                       
-- data Unit = C::Client | I::Invoice | P::Payment -- something like this ???
                       
{------------------------------------------------------------------------------

parseMaybe p =
  (do n <- p
      return (Just n))
  <|> return Nothing

------------------------------------------------------------------------------}

eol :: Parser ()
eol = do
    oneOf "\n\r"
    return ()
  <?> "end of line"

comment :: Parser ()
comment = do
    char '-'; char '-'
    skipMany (noneOf "\r\n")
  <?> "comment"

-- quoted item / name / text ???

name :: Parser String         
name = do
    char '"'
    str <- many (letter <|> digit <|> (oneOf ".&-_ "))
    char '"'
    return str
  <?> "name"
          
{------------------------------------------------------------------------------

numberF :: Parser Float
numberF = do
    n <- many1 (digit <|> char '.')
    if (head n) == '.' then return $ read $ '0':n 
                       else return $ read n 
  <?> "number"
         
pointedFloat :: Parser String 
pointedFloat = do
    dot <- char '.'
    n   <- many1 digit
    return $ '0':dot:n
  <?> "pointedFloat"            
                
intFloat :: Parser Float               
intFloat = do
    n <- many1 digit
    return $ read n
  <?> "intFloat"

------------------------------------------------------------------------------}

iFloat :: Parser Float               
iFloat = read <$> many1 digit <?> "iFloat"

pointFloat :: Parser Float
pointFloat = do
    n1:n2:[] <- sepBy (many digit) (oneOf ".,")
    return $ read $ (if null n1 then "0" else n1) ++ 
                    ('.':(if null n2 then "0" else n2))
  <?> "pointFloat"

parseFloat :: Parser Float
parseFloat = try pointFloat <|> iFloat
  <?> "parseFloat"     
  
-- parseDate ... can later directly convert to my Date type 
parseDate :: Parser UTCTime
parseDate = do
  d:m:y:[] <- sepBy (many1 digit) (oneOf "./|-")
  return $ UTCTime (fromGregorian (read y) (read m) (read d)) 
                   (timeOfDayToTime $ TimeOfDay 9 0 0)
            
clientA :: Parser Client  
clientA = Cl <$> (skipMany space *> char 'c' *> many letter *> skipMany space
              *> many digit)
             <*> (skipMany space *> name)
             <*> (skipMany space *> parseFloat)
             <*> (skipMany space *> parseFloat)
             <*> pure True
          <?> "clientA record"
  
client :: Parser Client
client = do
    skipMany space
    char 'c'
    many letter
    skipMany space
    rcIc <- many digit
    skipMany space
    clName <- name
    skipMany space
    intRate <- parseFloat
    skipMany space
    penRate <- parseFloat
    return $ Cl rcIc clName intRate penRate True
  <?> "client record"

{------------------------------------------------------------------------------
  testing parsers
------------------------------------------------------------------------------}

main = do
  let testString1 = " cli  26493381  \"Prvni Elektro a.s  \"  0.05         .10 "
      testString2 = " -- cli  26493381  \"Prvni_Elektro_a.s  \"  0.05         0.10 "
  putStrLn testString1
  putStrLn testString2
  case 
    parse (optional client) "client" testString1
    of
      Left err -> print err
      Right y  -> print y  
  case 
    runParser (optional clientA) () "client parse" testString1
    of
      Left err -> print err
      Right y  -> print y
  case 
    parse (optional clientA) "client.A" testString2
    of
      Left err -> print err
      Right y  -> print y  
  
  putStrLn "\nNo testing with Either ...\n"    
  x <- parseTest client testString1
  y <- parseTest client testString2
  print y
  