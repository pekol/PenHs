{------------------------------------------------------------------------------

  format-float.hs   Release 0.0.2
  -------------------------------
  pretty print floats

------------------------------------------------------------------------------}


import Data.List
import Control.Monad
import Text.Printf


ppFin :: Double -> String
ppFin f = ints ++ '.':fltp
  where
    ints = intercalate [','] . grp3Digits [] . reverse $ intp
    (intp, ('.':fltp)) = break (== '.')$ printf "%.2f" f
    grp3Digits ys xs
      | null xs = map reverse ys
      | otherwise = grp3Digits (pref:ys) suf
        where
          (pref, suf) = splitAt 3 xs 


main = do
  let w = [ 829745682.56843571, 0.21, 7456, 1, 397, 5681.4, 17862.1457 ] 
  print w
  mapM_ putStrLn $ map ppFin w
  

