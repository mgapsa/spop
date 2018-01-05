module Gui where
import Model

drawBoard :: Board -> IO ()
drawBoard board = do
  putStrLn "START"  
  drawBoard' board (0,0)
  drawBoard'' board (0,0)
  putStrLn "KONIEC"  

drawBoard' :: Board -> Position -> IO ()
drawBoard' board pos = do putStrLn "A"
                                    
drawBoard'' (Board r c ps) pos | elem pos ps = do putStrLn "A"
                               | otherwise = do putStrLn "B"
                                                         
drawBoard''' (Board rows cols poss) = show rows                           
