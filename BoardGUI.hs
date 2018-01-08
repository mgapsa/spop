module BoardGUI where
  import Board

  fieldTypeToString :: FieldType -> String
  fieldTypeToString House = "H"
  fieldTypeToString Gas = "G"
  fieldTypeToString Empty = "X"
  fieldTypeToString None = "-"

  drawMap :: Map -> IO ()
  drawMap [] = putStr "\n"
  drawMap (m:map) = do
    drawRow m
    drawMap map

  drawRow :: [FieldType] -> IO ()
  drawRow [] = putStr "\n"
  drawRow (e:elem) = do
    putStr (fieldTypeToString e)
    drawRow elem
