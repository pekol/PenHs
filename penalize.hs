{------------------------------------------------------------------------------
  penalize.hs           Release 0.0.3
  -----------------------------------
  kalkulace uroku z prodleni a pokuty
------------------------------------------------------------------------------}

module Interest where

import Data.Maybe
import qualified Data.Map as M
import Data.List (sort, sortBy)
import Control.Monad
import System.Locale
import System.Time
import Data.Time
import Data.Time.Clock.POSIX
import Text.Printf
import Data.Ord
import System.Environment

newtype Date = Dt { getDate :: UTCTime }
              deriving (Eq,Ord)
                       
instance Show Date where  
  show = formatTime defaultTimeLocale "%d/%m/%Y" . getDate

data Client = Cl { clientNo :: String, getClientName :: String,
                   getIntrP :: Float, getPenP :: Float, dailyP :: Bool }
              deriving (Show,Eq)
                       
data Invoice = Inv { invNo :: String, pmtDate :: Date, 
                     getAmtI :: Float, clNo :: String } 
              deriving (Show)               

data Payments = Pmt { invNoP :: String, pmtDateP :: Date, getAmtP :: Float,
                    prevDateP :: Date, getBal :: Float, prevBal :: Float }
              deriving (Show)

data Dues = Dues { invNoD :: String, from :: Date, to :: Date, days :: Integer,
                   getAmtD :: Float, getInterest :: Float, getPenalty :: Float }
              deriving (Show)                   

data Penalties = Pen { inPen, penPen, totPen :: Float }
              deriving (Show,Eq,Ord)

{------------------------------------------------------------------------------
date functions
------------------------------------------------------------------------------}

today :: IO UTCTime                                
today = getCurrentTime                    

mkDateUTC :: Int -> Int -> Integer -> UTCTime
mkDateUTC d m y = UTCTime
                  (fromGregorian y m d)
                  (timeOfDayToTime $ TimeOfDay 9 0 0)

dayDiff :: UTCTime -> UTCTime -> Integer         
dayDiffUTC d1 d2 = truncate $ diffUTCTime d1 d2 / posixDayLength                  
dayDiff = dayDiffUTC  

addDaysUTC :: NominalDiffTime -> UTCTime -> UTCTime
addDaysUTC days = addUTCTime (days * posixDayLength)
addDays = addDaysUTC

parseDateMyUTC :: String -> Maybe UTCTime
parseDateMyUTC d = parseTime defaultTimeLocale "%d/%m/%Y" d :: Maybe UTCTime
pDate = parseDateMyUTC

mkDate :: String -> Date
mkDate = Dt . fromJust . pDate
-- let y = mkDate "05/01/2012"  ==>  attention : "5/1/2012" wrong date !!!

{------------------------------------------------------------------------------
 reading from file and building Map's
------------------------------------------------------------------------------}

notVoid :: [Char] -> Bool
notVoid "" = False
notVoid ('-':_) = False
notVoid (' ':_) = False
notVoid ('\n':_) = False
notVoid txt 
  | length (words txt) < 4 = False
  | otherwise              = True

getData :: String -> [[String]]
getData = map words . filter notVoid . lines

fGetData :: FilePath -> IO [[String]]
fGetData = liftM getData . readFile

isFirstW :: Eq a => a -> [a] -> Bool
isFirstW _ [] = False  
isFirstW y (x:xs) = y == x
-- isFirstW y xs = head xs == y 

-- Pattern match for record elements ??
translClients :: [[String]] -> M.Map String Client
translClients xs = M.fromList $ map (\x -> (clientNo x,x)) cs
  where
    cs   = map mkCl $ filter (isFirstW "cli") xs
    mkCl (_:a:b:c:d:_) = Cl a b (read c) (read d) True  

translDebits :: [[String]] -> M.Map String Invoice
translDebits xs = M.fromList $ map (\x -> (invNo x,x)) ds
  where
    ds   = map mkDeb $ filter (isFirstW "deb") xs
    mkDeb (_:a:b:c:d:_) = Inv a (mkDate b) (read c) d
  
-- helper ordering functions for sorting
ascending :: Ord a => a -> a -> Ordering
ascending = compare
descending :: Ord a => a -> a -> Ordering
descending = flip compare
  
translPayments :: [[String]] -> M.Map String [Payments]
translPayments xs = M.fromListWith (++) $ map (\x -> (invNoP x,[x])) 
                    $ sortBy (comP descending) ps
  where
    -- comP is compare function for Payments records based on pmtDateP
    comP fComp a b   = fComp (pmtDateP a) (pmtDateP b)
    ps               = map mkPmt $ filter (isFirstW "pmt") xs
    mkPmt(_:a:b:c:_) = Pmt a (mkDate b) (read c) (mkDate "01/01/2112") 0 0

-- calculate period intervals from - to dates + balances & previous balances
-- calculating balances / transforming Payments records
calcBals :: Ord k =>
     UTCTime 
     -> M.Map k Invoice
     -> M.Map k [Payments]
     -> M.Map k [Payments]
calcBals td iM pM = M.mapWithKey f pM
  where
    f key x = mapBal d bal x []
      where
        Just inv = M.lookup key iM
        bal      = getAmtI inv
        d        = pmtDate inv
        mapBal d b (p:ps) pps
          | null ps   = reverse
              ((Pmt (invNoP p) (Dt td) (getAmtP p) (pmtDateP p) b' b):p':pps)
              -- Pmt ... added Pmt record for period from last pmt till today
          | otherwise = mapBal d' b' ps (p':pps)
          where 
            b' = b - getAmtP p
            d' = pmtDateP p
            p' = Pmt (invNoP p) (pmtDateP p) (getAmtP p) d  b b'
            
calcDues :: M.Map String Client
  -> M.Map String Invoice
  -> M.Map String [Payments]
  -> M.Map String [Dues]     
calcDues cM iM pM  = M.mapWithKey f pM
  where
    f key x = map (mkDues key) x
    mkDues k x = Dues k (prevDateP x) (pmtDateP x) days (getBal x) intD penD
      where
        Just cli = liftM clNo (lookup iM (invNoP x)) >>= lookup cM 
          where lookup = flip M.lookup 
        days     = dayDiff (getDate (pmtDateP x)) (getDate (prevDateP x))
        intD     = calc (getIntrP cli) (getBal x) days
        penD     = calc (getPenP cli)  (getBal x) days 

{------------------------------------------------------------------------------
  presenting functions
------------------------------------------------------------------------------}

showP :: Payments -> String
showP p = printf "fa: %-9s Pmt: %-9s Prev: %-9s Pmt:%9.2f Bal: %5.2f  Prev: %5.2f"   
          (invNoP p) (show (pmtDateP p)) (show (prevDateP p))
          (getAmtP p) (getBal p) (prevBal p)

showD :: Dues -> String
showD d = printf "fa: %-9s od: %-9s do: %-9s Bal:%8.0f Int: %7.2f  Pen: %7.2f"   
          (invNoD d) (show (from d)) (show (to d)) 
          (getAmtD d) (getInterest d) (getPenalty d)

reportMaps :: (a -> String) -> M.Map k [a] -> String
reportMaps showFunc x = 
  let ls = concat $ M.elems x
      l2 = map showFunc ls
  in unlines l2

{------------------------------------------------------------------------------
  interest and penalty and total summary functions
------------------------------------------------------------------------------}

totDues :: M.Map k [Dues] -> Penalties
totDues xD =
  let ls = concat $ M.elems xD
      ff l (t1,t2) = (t1 + getInterest l, t2 + getPenalty l)
      (t1,t2) = foldr ff (0,0) ls
  in  Pen t1 t2 (t1 + t2)

totalPen :: M.Map k [Dues] -> Penalties
totalPen xD = Pen i p (i+p)
  where
    (i, p)  = M.fold (\x (i,p) -> f (i,p) x) (0,0) xD
    f (i,p) = foldr (\x (i,p) -> (i + getInterest x, p + getPenalty x)) (i,p)

calcInterestDaily :: Float -> Float -> Integer -> Float
calcInterestDaily rate amt days = rate * (fromIntegral days::Float) * amt / 100
calc = calcInterestDaily

{------------------------------------------------------------------------------
  almost main function
------------------------------------------------------------------------------}

-- mapping :: [Char] -> [Char] -> IO (M.Map String [Dues])
mapping :: [[String]] -> UTCTime -> IO ()
mapping rawData td = do
  let cliMap = translClients rawData
      invMap = translDebits rawData
      pmtMp' = translPayments rawData
      -- calculating balances / transforming Payments records
      pmtMap = calcBals td invMap pmtMp'
      dueMap = calcDues cliMap invMap pmtMap
  putStrLn ""
  putStrLn $ reportMaps showD dueMap
  print $ totDues dueMap
  putStrLn ""
--  return dueMap
  
{------------------------------------------------------------------------------
some testing
------------------------------------------------------------------------------}

mainTest = do
  now <- getClockTime
  tdate <- toCalendarTime now
  print tdate
  let nowUTC = toUTCTime now
      yr = show $ ctYear nowUTC
  print nowUTC    
  putStrLn $ "UTC Yr : " ++ yr
  print nowUTC
  print $ nowUTC {ctYear = 2010}
  putStrLn ""
  
main = do
  args <- getArgs
  case args of
    []      -> do
      rawData <- fGetData "prvni-elektro.pmt"      
      td      <- today     
      putStrLn "\nSpustit jako : pen jmeno_souboru datum -> ve formatu \"01/02/2012\"\n"
      mapping rawData td      
    (f:[])  -> do
      rawData <- fGetData f      
      td      <- today
      mapping rawData td
    (f:d:[]) -> do  
      rawData <- fGetData f      
      td      <- return $ fromJust $ pDate d
      mapping rawData td
    otherwise -> putStrLn "\nSpustit jako : pen jmeno_souboru datum /ve formatu \"01/02/2012\"\n"