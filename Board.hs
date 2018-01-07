module Board where
  -- Point = (Row, Column)
  type Point = (Int, Int)

  data Board = Board {rows::[Int],
                      cols::[Int],
                      houses::[Point]} deriving (Show, Read)

  data FieldType = Empty | House | Gas deriving Eq
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
  isHouse [] _ = Empty
  isHouse (h:hs) position
    | h == position = House
    | otherwise = isHouse hs position

  getFieldTypeAt :: Map -> Point -> FieldType
  getFieldTypeAt [] _ = Empty
  getFieldTypeAt map (x, y) = elem where
    row = map !! y
    elem = row !! x


  putFieldAt :: Map -> Point -> FieldType -> [Int] -> [Int] -> Map
  putFieldAt map (x, y) field rows cols = setMap map (x, y) field rows cols

  setMap :: Map -> Point -> FieldType -> [Int] -> [Int] -> Map
  setMap map (r,c) field rows cols = newMap where
    xSize = length cols
    ySize = length rows
    newMap = setRow map 0 (xSize, ySize) (r, c) field

  setRow :: Map -> Int -> Point -> Point -> FieldType -> Map
  setRow map rowIdx (xSize, ySize) (r, c) field
    | rowIdx < ySize = row : setRow map (rowIdx+1) (xSize, ySize) (r, c) field
    | otherwise = [[]]
    where
      row = setElement map rowIdx 0 (xSize, ySize) (r, c) field

  setElement :: Map -> Int -> Int -> Point -> Point -> FieldType -> [FieldType]
  setElement map rowIdx colIdx (xSize, ySize) (r, c) field
    | colIdx < xSize = element : setElement map rowIdx (colIdx+1) (xSize, ySize) (r, c) field
    | otherwise = []
    where
      element | rowIdx == r && colIdx == c = field
              | otherwise = (map !! rowIdx) !! colIdx
