module BoardGUI where
  import Board
  import Data.Char

  fieldTypeToString :: FieldType -> String
  fieldTypeToString House = "H"
  fieldTypeToString Gas = "G"
  fieldTypeToString Empty = "X"
  fieldTypeToString None = "-"

  drawBoard :: Board -> IO ()
  drawBoard board = do
    putStr " "
    printElements (cols board)
    putStr "\n"
    drawBoardImpl (mapa board) (rows board)

  drawBoardImpl :: MapType -> [Int] -> IO ()
  drawBoardImpl [] _ = putStr "\n"
  drawBoardImpl _ [] = putStr "\n"
  drawBoardImpl (m:map) (c:col) = do
    putStr [chr (c+48)]
    drawRow m
    drawBoardImpl map col

  drawRow :: [FieldType] -> IO ()
  drawRow [] = putStr "\n"
  drawRow (e:elem) = do
    putStr (fieldTypeToString e)
    drawRow elem

  printElements :: [Int] -> IO()
  printElements [] = return ()
  printElements (x:xs) = do putStr [chr (x+48)]
                            printElements xs


  writeBoardToFile :: Board -> FilePath -> IO ()
  writeBoardToFile b file = do writeFile file (writeMapToFile (mapa b))

  writeMapToFile :: MapType -> [Char]
  writeMapToFile [] = ""
  writeMapToFile (row:rows) = writeMapRowToFile row ++ "\n" ++ (writeMapToFile rows)

  writeMapRowToFile :: [FieldType] -> [Char]
  writeMapRowToFile [] = ""
  writeMapRowToFile (e:elem) = fieldTypeToString e ++ writeMapRowToFile elem
