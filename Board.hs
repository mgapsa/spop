module Board where
  -- Point = (Column, Row)
  type Point = (Int, Int)

  data FieldType = None | Empty | House | Gas deriving Eq
  type MapType = [[FieldType]]

  data Board = Board {mapa::MapType,
                      rows::[Int],
                      cols::[Int],
                      houses::[Point]}

  generateMap :: [Int] -> [Int] -> [Point] -> MapType
  generateMap rows cols houses = mapa where
    xSize = length cols
    ySize = length rows
    mapa = generateRow houses 0 (xSize, ySize)

  generateRow :: [Point] -> Int -> Point -> MapType
  generateRow houses rowIdx (xSize, ySize)
    | rowIdx < ySize = row : generateRow houses (rowIdx+1) (xSize, ySize)
    | otherwise = [[]]
    where
      row = generateElement houses rowIdx 0 (xSize, ySize)

  generateElement :: [Point] -> Int -> Int -> Point -> [FieldType]
  generateElement houses rowIdx colIdx (xSize, ySize)
    | colIdx < xSize = house : generateElement houses rowIdx (colIdx+1) (xSize, ySize)
    | otherwise = []
    where
      house = isHouse houses (rowIdx,colIdx)

  isHouse :: [Point] -> Point -> FieldType
  isHouse [] _ = None
  isHouse (h:hs) housePosition
    | h == housePosition = House
    | otherwise = isHouse hs housePosition

  getFieldTypeAt :: Board -> Point -> FieldType
  getFieldTypeAt board (colIdx, rowIdx)
    | rowIdx < 0 || rowIdx >= rowSize || colIdx < 0 || colIdx >= colSize = None
    | otherwise = (m !! rowIdx) !! colIdx
    where
      m = mapa board
      rowSize = length m
      colSize = length (m !! 0)

  putFieldAt :: Board -> Point -> FieldType -> Board
  putFieldAt board (dstColIdx,dstRowIdx) field = updatedBoard where
    updatedBoard = (Board m r c h)
    m = setMap (mapa board) (dstColIdx,dstRowIdx) field r c
    r = rows board
    c = cols board
    h = houses board

  setMap :: MapType -> Point -> FieldType -> [Int] -> [Int] -> MapType
  setMap mapa (dstColIdx,dstRowIdx) field rows cols = newBoard where
    colSize = length cols
    rowSize = length rows
    newBoard = setRow mapa 0 (colSize,rowSize) (dstColIdx,dstRowIdx) field

  setRow :: MapType -> Int -> Point -> Point -> FieldType -> MapType
  setRow mapa rowIdx (colSize,rowSize) (dstColIdx,dstRowIdx) field
    | rowIdx < rowSize = row : setRow mapa (rowIdx+1) (colSize,rowSize) (dstColIdx,dstRowIdx) field
    | otherwise = [[]]
    where
      row = setElement mapa 0 rowIdx (colSize,rowSize) (dstColIdx,dstRowIdx) field

  setElement :: MapType -> Int -> Int -> Point -> Point -> FieldType -> [FieldType]
  setElement mapa colIdx rowIdx (colSize, rowSize) (dstColIdx,dstRowIdx) field
    | colIdx < colSize = element : setElement mapa (colIdx+1) rowIdx (colSize,rowSize) (dstColIdx,dstRowIdx) field
    | otherwise = []
    where
      element | rowIdx == dstRowIdx && colIdx == dstColIdx = field
              | otherwise = (mapa !! rowIdx) !! colIdx

  -- Zwraca indeks kolejnego pustego pola (począwszy od n+1)
  nextEmpty :: Board -> Int -> Point
  nextEmpty b n | n >= (length(rows b) * length(cols b) - 1) = (-1, -1) -- zwróć (-1, -1)) jeśli nie znaleziono żadnego pustego pola
                 | isEmptyAt b (n2xy b (n+1))   = n2xy b (n+1)
                 | otherwise                    = nextEmpty b (n+1)


  -- Zwraca współrzędne pola o podanym indeksie linowym
  n2xy :: Board -> Int -> (Int, Int)
  n2xy b n = ((mod n (length(cols b))), (div n (length(rows b))))
  -- Zwraca indeks linowy pola o podanych współrzędnych
  xy2n :: Board -> (Int, Int) -> Int
  xy2n b (x, y) = y * length(cols b) + x


  isEmptyAt :: Board -> Point -> Bool
  isEmptyAt board point = getFieldTypeAt board point == Empty
