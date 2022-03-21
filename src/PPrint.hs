{-
  Blockchain implementation
  @author Resul Hangeldiyev (rh402185)
-}

module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v)
  = showString k
  . showString ": "
  . shows v

pprH, pprV :: [ShowS] -> ShowS
pprV l = intercalateS (showChar '\n') l
pprH l = intercalateS (showChar ' ') l

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS _ [] = showString ""
intercalateS _ [x] = x
intercalateS sep (x : xs) = x . sep . intercalateS sep xs

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith _ [] = showString ""
pprListWith f [x] = f x
pprListWith f (x : xs) = f x . showChar '\n' . pprListWith f xs

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
