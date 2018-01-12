module BoardGUI where
  import Board
  import Data.Char

  fieldTypeToString :: FieldType -> String
  fieldTypeToString House = "H"
  fieldTypeToString Gas = "G"
  fieldTypeToString Empty = "X"
  fieldTypeToString None = "-"

  drawMap :: Board -> IO ()
  drawMap board = do
    putStr " "
    printElements (cols board)
    putStr "\n"
    drawMapImpl (mapa board) (rows board)

  drawMapImpl :: MapType -> [Int] -> IO ()
  drawMapImpl [] _ = putStr "\n"
  drawMapImpl _ [] = putStr "\n"
  drawMapImpl (m:map) (c:col) = do
    putStr [chr (c+48)]
    drawRow m
    drawMapImpl map col

  drawRow :: [FieldType] -> IO ()
  drawRow [] = putStr "\n"
  drawRow (e:elem) = do
    putStr (fieldTypeToString e)
    drawRow elem

  printElements :: [Int] -> IO()
  printElements [] = return ()
  printElements (x:xs) = do putStr [chr (x+48)]
                            printElements xs
