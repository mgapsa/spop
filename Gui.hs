module Gui where
import Model

drawBoard :: Board -> IO ()
drawBoard board = do
  putStrLn "START"  
  drawBoard' board (0,0)
  putStrLn "KONIEC"  

drawBoard' :: Board -> Position -> IO ()
drawBoard' (Board rows cols poss) pos | elem pos poss = do 
                                         putStr $ ("A") ++ " "
                                      | otherwise = do 
                                         putStr $ ("X") ++ " "


-- drawBoard' plaster pos 
--   | pos == maxPos plaster = do 
--         putStr $ (stringLetter plaster pos) ++ " "
--         return "ok"
--   | isEvenRow pos && isFirstInRow plaster pos = do 
--         putStr $ " " ++ (stringLetter plaster pos) ++ " "
--         drawBoard' plaster (nextPos plaster pos)     
--   | isLastInRow plaster pos = do 
--         putStrLn $ (stringLetter plaster pos) ++ " "
--         drawBoard' plaster (nextPos plaster pos)  
--   | otherwise = do 
--         putStr $ (stringLetter plaster pos) ++ " "
--         drawBoard' plaster (nextPos plaster pos)