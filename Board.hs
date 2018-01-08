module Board where
  -- Point = (Column, Row)
  type Point = (Int, Int)

  data Board = Board {rows::[Int],
                      cols::[Int],
                      houses::[Point]} deriving (Show, Read)

  data FieldType = None | Empty | House | Gas deriving Eq
  type Map = [[FieldType]]

  getMap :: Board -> Map
  getMap (Board rows cols houses) = map where
    xSize = length cols
    ySize = length rows
    map = getRow houses 0 (xSize, ySize)

  getRow :: [Point] -> Int -> Point -> Map
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

  getFieldTypeAt :: Map -> Point -> FieldType
  getFieldTypeAt [] _ = None
  getFieldTypeAt map (colIdx, rowIdx)
    | rowIdx < 0 || rowIdx >= rowSize || colIdx < 0 || colIdx >= colSize = None
    | otherwise = (map !! rowIdx) !! colIdx
    where
      rowSize = length map
      colSize = length (map !! 0)


  putFieldAt :: Map -> Point -> FieldType -> [Int] -> [Int] -> Map
  putFieldAt map (dstColIdx,dstRowIdx) field rows cols
    = setMap map (dstColIdx,dstRowIdx) field rows cols

  setMap :: Map -> Point -> FieldType -> [Int] -> [Int] -> Map
  setMap map (dstColIdx,dstRowIdx) field rows cols = newMap where
    colSize = length cols
    rowSize = length rows
    newMap = setRow map 0 (colSize,rowSize) (dstColIdx,dstRowIdx) field

  setRow :: Map -> Int -> Point -> Point -> FieldType -> Map
  setRow map rowIdx (colSize,rowSize) (dstColIdx,dstRowIdx) field
    | rowIdx < rowSize = row : setRow map (rowIdx+1) (colSize,rowSize) (dstColIdx,dstRowIdx) field
    | otherwise = [[]]
    where
      row = setElement map 0 rowIdx (colSize,rowSize) (dstColIdx,dstRowIdx) field

  setElement :: Map -> Int -> Int -> Point -> Point -> FieldType -> [FieldType]
  setElement map colIdx rowIdx (colSize, rowSize) (dstColIdx,dstRowIdx) field
    | colIdx < colSize = element : setElement map (colIdx+1) rowIdx (colSize,rowSize) (dstColIdx,dstRowIdx) field
    | otherwise = []
    where
      element | rowIdx == dstRowIdx && colIdx == dstColIdx = field
              | otherwise = (map !! rowIdx) !! colIdx
