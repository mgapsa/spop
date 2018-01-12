module Board where
  -- Point = (Column, Row)
  type Point = (Int, Int)

  data FieldType = None | Empty | House | Gas deriving Eq
  type MapType = [[FieldType]]

  data Board = Board {mapa::MapType,
                      rows::[Int],
                      cols::[Int],
                      houses::[Point]}

  getMap :: [Int] -> [Int] -> [Point] -> MapType
  getMap rows cols houses = mapa where
    xSize = length cols
    ySize = length rows
    mapa = getRow houses 0 (xSize, ySize)

  getRow :: [Point] -> Int -> Point -> MapType
  getRow houses rowIdx (xSize, ySize)
    | rowIdx < ySize = row : getRow houses (rowIdx+1) (xSize, ySize)
    | otherwise = [[]]
    where
      row = getElement houses rowIdx 0 (xSize, ySize)

  getElement :: [Point] -> Int -> Int -> Point -> [FieldType]
  getElement houses rowIdx colIdx (xSize, ySize)
    | colIdx < xSize = house : getElement houses rowIdx (colIdx+1) (xSize, ySize)
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


  putFieldAt :: Board -> Point -> FieldType -> [Int] -> [Int] -> Board
  putFieldAt board (dstColIdx,dstRowIdx) field rows cols
    = (Board (setMap (mapa board) (dstColIdx,dstRowIdx) field rows cols) rows cols (houses board))

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
