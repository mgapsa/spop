module BoardGUI where
  import Board
  import Data.Char

  -- konwertuje typ pola na string
  -- przeznaczony do wypisania na standardowe wyjscie
  fieldTypeToString :: FieldType -> String
  fieldTypeToString House = "H"
  fieldTypeToString Gas = "G"
  fieldTypeToString Empty = "X"
  fieldTypeToString None = "-"

  -- wypisuje zagadke na standardowe wyjscie
  drawBoard :: Board -> IO ()
  drawBoard board = do
    putStr " "
    printElements (cols board)
    putStr "\n"
    drawBoardImpl (mapa board) (rows board)

  -- implementuje drawBoard
  -- wypisuje zagadke wiersz po wierszu
  drawBoardImpl :: MapType -> [Int] -> IO ()
  drawBoardImpl [] _ = putStr "\n"
  drawBoardImpl _ [] = putStr "\n"
  drawBoardImpl (m:map) (c:col) = do
    putStr [chr (c+48)]
    drawRow m
    drawBoardImpl map col

  -- wypisuje zagadke element po elemencie w danym wierszu
  drawRow :: [FieldType] -> IO ()
  drawRow [] = putStr "\n"
  drawRow (e:elem) = do
    putStr (fieldTypeToString e)
    drawRow elem

  -- wypisuje elementy z listy intow
  printElements :: [Int] -> IO()
  printElements [] = return ()
  printElements (x:xs) = do putStr [chr (x+48)]
                            printElements xs

  -- zapisuje zagadke do pliku
  writeBoardToFile :: Board -> FilePath -> IO ()
  writeBoardToFile b file = do writeFile file (writeMapToFile (mapa b))

  writeMapToFile :: MapType -> [Char]
  writeMapToFile [] = ""
  writeMapToFile (row:rows) = writeMapRowToFile row ++ "\n" ++ (writeMapToFile rows)

  writeMapRowToFile :: [FieldType] -> [Char]
  writeMapRowToFile [] = ""
  writeMapRowToFile (e:elem) = fieldTypeToString e ++ writeMapRowToFile elem
