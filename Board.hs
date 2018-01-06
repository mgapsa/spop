module Board where
  -- Point = (Row, Column)
  type Point = (Int, Int)

  data Board = Board {rows::[Int],
                      cols::[Int],
                      houses::[Point]} deriving (Show, Read)

  data FieldType = Empty | House | Gas
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
  getElement houses rowIdx col (xSize, ySize)
    | col < xSize = house : getElement houses rowIdx (col+1) (xSize, ySize)
    | otherwise = []
    where
      house = isHouse houses (rowIdx,col)

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
