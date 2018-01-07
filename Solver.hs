module Solver where
  import Board

  solve :: Map -> Board -> Map
  solve map (Board rows cols houses) = solveImpl map (0,0) rows cols

  solveImpl :: Map -> Point -> [Int] -> [Int] -> Map
  solveImpl map (c , r) rows cols
    | r > (length rows -1) = map
    | r < (length rows -1) && c > (length cols -1) = solveImpl map (0, r+1) rows cols
    | (getFieldTypeAt map (r,c)) == House && c >= (length cols -1) = solveImpl (putGasNextToHouse map (c , r) rows cols) (0, r+1) rows cols
    | (getFieldTypeAt map (r,c)) == House && r >= (length rows -1) && c >= (length cols -1) = putGasNextToHouse map (c , r) rows cols
    | (getFieldTypeAt map (r,c)) == House = solveImpl (putGasNextToHouse map (c , r) rows cols) (c+1, r) rows cols
    | r >= (length rows -1) && c >= (length cols -1) = map
    | c >= (length cols -1) = solveImpl map (0, r+1) rows cols
    | otherwise = solveImpl map (c+1, r) rows cols

  --dodac sprawdzanie w jednej linii i jakies returny jak slepy zaulek
  putGasNextToHouse :: Map -> Point -> [Int] -> [Int] -> Map
  putGasNextToHouse map (c , r) rows cols = putFieldAt map (c , r) Gas rows cols