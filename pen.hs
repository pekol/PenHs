{------------------------------------------------------------------------------

  pen.hs        Release 0.0.3
  ---------------------------
  main module for penalize.hs

------------------------------------------------------------------------------}  


module Main where  

import Interest hiding (main)
import System.Environment
import Control.Monad
import Data.Maybe

main = do
  a <- getArgs
  case length a of
    2 -> do
      rd <- fGetData (a!!0)
      td <- return $ fromJust $ pDate (a!!1)
      mapping rd td
    1 -> do
      rd <- fGetData (a!!0)
      td <- today
      mapping rd td
    0 -> do
      rd <- fGetData "prvni.pmt"
      td <- today
      putStrLn "\nZadat nazev souboru a datum kalkulace :"
      putStrLn $ "pen prvni.pmt " ++ show (Dt td)  ++ "\n"        
      mapping rd td


