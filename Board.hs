module Board where
  -- Position = (Row, Column)
  type Position = (Int, Int)

  data Board = Board {rows::[Int],
                      cols::[Int],
                      houses::[Position]} deriving (Show, Read)
