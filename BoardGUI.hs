module BoardGUI where
  import Board

  drawBoard :: Board -> IO ()
  drawBoard board = do
    putStrLn "DEBUG: START drawBoard"
    drawBoard' board (0,0)
    drawBoard'' board (0,0)
    putStrLn "DEBUG: END drawBoard"

  drawBoard' :: Board -> Position -> IO ()
  drawBoard' board pos = do putStrLn "A"

  drawBoard'' (Board r c ps) pos | elem pos ps = do putStrLn "A"
                                | otherwise = do putStrLn "B"

  drawBoard''' (Board rows cols poss) = show rows
