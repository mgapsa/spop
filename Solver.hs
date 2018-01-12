module Solver where
  import Board

  solve :: Map -> Board -> Map
  solve map (Board rows cols houses) = solvedMap where
    map1 = filterDistantFields map (0,0) rows cols
    updatedMap = runHeuristics map1 rows cols houses
    solvedMap = updatedMap

  runHeuristics :: Map -> [Int] -> [Int] -> [Point] -> Map
  runHeuristics map0 rows cols houses = finalMap where
    map1 = filterEmptyRowsColumns map0 (0,0) rows cols
    finalMap = map1

  solveImpl :: Map -> Point -> [Int] -> [Int] -> Map
  solveImpl map (c , r) rows cols
    | r > (length rows -1) = map
    | r < (length rows -1) && c > (length cols -1) = solveImpl map (0, r+1) rows cols
    | (getFieldTypeAt map (c,r)) == House && c >= (length cols -1) = solveImpl (putGasNextToHouse map (c , r) rows cols) (0, r+1) rows cols
    | (getFieldTypeAt map (c,r)) == House && r >= (length rows -1) && c >= (length cols -1) = putGasNextToHouse map (c , r) rows cols
    | (getFieldTypeAt map (c,r)) == House = solveImpl (putGasNextToHouse map (c , r) rows cols) (c+1, r) rows cols
    | r >= (length rows -1) && c >= (length cols -1) = map
    | c >= (length cols -1) = solveImpl map (0, r+1) rows cols
    | otherwise = solveImpl map (c+1, r) rows cols

  --dodac sprawdzanie w jednej linii i jakies returny jak slepy zaulek
  putGasNextToHouse :: Map -> Point -> [Int] -> [Int] -> Map
  putGasNextToHouse map (c , r) rows cols = putFieldAt map (c , r) Gas rows cols

  filterEmptyRowsColumns :: Map -> Point -> [Int] -> [Int] -> Map
  filterEmptyRowsColumns map (colIdx, rowIdx) rows cols
    | rowIdx < length rows && colIdx < length cols = finalMap
    | rowIdx < length rows = filterEmptyRowsColumns map (0,rowIdx+1) rows cols
    | otherwise = map
    where
      updatedMap = putFieldAt map (colIdx,rowIdx) field rows cols
      finalMap = filterEmptyRowsColumns updatedMap (colIdx+1,rowIdx) rows cols
      field
        | ((rows !! rowIdx) == 0 || (cols !! colIdx) == 0) = Empty
        | otherwise = (map !! rowIdx) !! colIdx

  filterDistantFields :: Map -> Point -> [Int] -> [Int] -> Map
  filterDistantFields map (colIdx,rowIdx) rows cols
    | rowIdx < length rows && colIdx < length cols = finalMap
    | rowIdx < length rows = filterDistantFields map (0,rowIdx+1) rows cols
    | otherwise = map
    where
      updatedMap = putFieldAt map (colIdx,rowIdx) field rows cols
      finalMap = filterDistantFields updatedMap (colIdx+1,rowIdx) rows cols
      field
        | getFieldTypeAt map (colIdx,rowIdx) == None
          && (getFieldTypeAt map (colIdx,rowIdx-1) == House
            || getFieldTypeAt map (colIdx-1,rowIdx) == House
            || getFieldTypeAt map (colIdx+1,rowIdx) == House
            || getFieldTypeAt map (colIdx,rowIdx+1) == House) == False = Empty
        | otherwise = (map !! rowIdx) !! colIdx
