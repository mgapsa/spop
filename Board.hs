module Board where
  -- Point = (Column, Row)
  type Point = (Int, Int)

  data FieldType = None | Empty | House | Gas deriving Eq
  type MapType = [[FieldType]]

  data Board = Board {mapa::MapType,
                      rows::[Int],
                      cols::[Int],
                      houses::[Point],
                      rowsBase::[Int],
                      colsBase::[Int]} deriving (Eq)

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
    | rowIdx < 0 || rowIdx >= length r || colIdx < 0 || colIdx >= length c = None
    | otherwise = (m !! rowIdx) !! colIdx
    where
      m = mapa board
      r = rows board
      c = cols board

  putFieldAt :: Board -> Point -> FieldType -> Board
  putFieldAt board (dstColIdx,dstRowIdx) field
    | field == Gas = finalBoard
    | otherwise = updatedBoard
    where
      finalBoard = filterGasTankProximity updatedBoard (dstColIdx,dstRowIdx)
      updatedBoard = board {mapa = m, rows = r, cols = c}
      m = setMap (mapa board) (dstColIdx,dstRowIdx) field r c
      r = if field == Gas && getFieldTypeAt board (dstColIdx,dstRowIdx) /= Gas
          then decrementElementInList (rows board) dstRowIdx
          else rows board
      c = if field == Gas && getFieldTypeAt board (dstColIdx,dstRowIdx) /= Gas
          then decrementElementInList (cols board) dstColIdx
          else cols board
      h = houses board

  decrementElementInList :: [Int] -> Int -> [Int]
  decrementElementInList [] _ = []
  decrementElementInList (x:xs) idx
    | idx > 0 = x : decrementElementInList xs (idx-1)
    | otherwise = (x-1) : xs

  filterGasTankProximity :: Board -> Point -> Board
  filterGasTankProximity board (colIdx,rowIdx) = finalBoard where
    boardNW = filterGasTankProximityImpl board (colIdx-1,rowIdx-1)
    boardNX = filterGasTankProximityImpl boardNW (colIdx,rowIdx-1)
    boardNE = filterGasTankProximityImpl boardNX (colIdx+1,rowIdx-1)
    boardEX = filterGasTankProximityImpl boardNE (colIdx+1,rowIdx)
    boardSE = filterGasTankProximityImpl boardEX (colIdx+1,rowIdx+1)
    boardSX = filterGasTankProximityImpl boardSE (colIdx,rowIdx+1)
    boardSW = filterGasTankProximityImpl boardSX (colIdx-1,rowIdx+1)
    boardWX = filterGasTankProximityImpl boardSW (colIdx-1,rowIdx)
    finalBoard = boardWX

  filterGasTankProximityImpl :: Board -> Point -> Board
  filterGasTankProximityImpl board (colIdx,rowIdx)
    | colIdx >= 0 && rowIdx >= 0 && colIdx < length c && rowIdx < length r = finalBoard
    | otherwise = board
    where
      c = cols board
      r = rows board
      isNone = (getFieldTypeAt board (colIdx,rowIdx) == None)
      finalBoard = if isNone
                   then putFieldAt board (colIdx,rowIdx) Empty
                   else board

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

  --rows to jest to w pionie
  -- Zwraca współrzędne pola o podanym indeksie linowym
  n2xy :: Board -> Int -> (Int, Int)
  n2xy b n = ((mod n (length(cols b))), (div n (length(cols b))))
  -- Zwraca indeks linowy pola o podanych współrzędnych
  xy2n :: Board -> (Int, Int) -> Int
  xy2n b (x, y) = y * length(cols b) + x


  isEmptyAt :: Board -> Point -> Bool
  isEmptyAt board point = getFieldTypeAt board point == None


  countSthInRow :: Board -> Point -> FieldType -> Int
  countSthInRow board (colIdx,rowIdx) filed
    | colIdx < length c = val + countSthInRow board (colIdx+1,rowIdx) filed
    | otherwise = 0
    where
      c = cols board
      val | getFieldTypeAt board (colIdx,rowIdx) == filed = 1
          | otherwise = 0

  countSthInCol :: Board -> Point -> FieldType -> Int
  countSthInCol board (colIdx,rowIdx) field
    | rowIdx < length r = val + countSthInCol board (colIdx,rowIdx+1) field
    | otherwise = 0
    where
      r = rows board
      val | getFieldTypeAt board (colIdx,rowIdx) == field = 1
          | otherwise = 0

  areRowsComplete :: Board -> Int -> Bool
  areRowsComplete board n | n == length(rows board) = True
                          | otherwise = ((rows board) !! n) == 0 && areRowsComplete board (n+1)

  areColsComplete :: Board -> Int -> Bool
  areColsComplete board n | n == length(cols board) = True
                          | otherwise = ((cols board) !! n) == 0 && areColsComplete board (n+1)

  isBoardComplete :: Board -> Bool
  isBoardComplete board = (areRowsComplete board 0) && (areColsComplete board 0)

  areRowsWithError :: Board -> Int -> Bool
  areRowsWithError board n | n == length(rows board) = False
                           | otherwise = ((countSthInRow board (0, n) Gas) > ((rowsBase board) !! n)) || areRowsWithError board (n+1)

  areColsWithError :: Board -> Int -> Bool
  areColsWithError board n | n == length(cols board) = False
                           | otherwise = (countSthInCol board (n, 0) Gas) > ((colsBase board) !! n) || areColsWithError board (n+1)

  isBoardWithError :: Board -> Bool
  isBoardWithError board = (areRowsWithError board 0) || (areColsWithError board 0)

  areThereAnyHousesWithoutGas :: Board -> Int -> Bool

