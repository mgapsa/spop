module Solver where
  import Board

  solve :: Board -> Board
  solve board = solvedBoard where
    board1 = filterDistantFields board (0,0)
    board2 = runHeuristics board1
    solvedBoard = findSolution board2

  -- --dodac sprawdzanie w jednej linii i jakies returny jak slepy zaulek
  -- putGasNextToHouse :: MapType -> Point -> [Int] -> [Int] -> MapType
  -- putGasNextToHouse map (c , r) rows cols = putFieldAt map (c , r) Gas

  filterEmptyRowsColumns :: Board -> Point -> Board
  filterEmptyRowsColumns board (colIdx, rowIdx)
    | rowIdx < length r && colIdx < length c = finalBoard
    | rowIdx < length r = filterEmptyRowsColumns board (0,rowIdx+1)
    | otherwise = board
    where
      m = mapa board
      r = rows board
      c = cols board
      updatedBoard = putFieldAt board (colIdx,rowIdx) field
      finalBoard = filterEmptyRowsColumns updatedBoard (colIdx+1,rowIdx)
      field
        | ((r !! rowIdx) == 0 || (c !! colIdx) == 0) = Empty
        | otherwise = (m !! rowIdx) !! colIdx

  -- Przeszukuje wgłąb przestrzeń rozwiązań wstawiając w wolne pola zbiorniki
  findSolution :: Board -> Board
  findSolution b = if sn /= (-1, -1) then findSolutionImpl sb ((xy2n b sn)-1) else sb
                    where sb = runHeuristics b
                          sn = nextEmpty sb (-1)
  findSolutionImpl :: Board -> Int -> Board                        
  findSolutionImpl b n | isBoardWithError bC = result_mark
                       | isBoardComplete bC  = bC
                       | otherwise           = if (mapa bC') == mapa (error_board)
                                               then result_mark
                                               else bC'
                       where bC = runHeuristics (putFieldAt b nextPoint Gas)
                             bC' = if nextPointCExists then (findSolutionImpl bC nextPointAsNumber) else error_board 
                             bM = runHeuristics (putFieldAt b nextPoint None)
                             bM' = if nextPointMExists then (findSolutionImpl bM nextPointAsNumber) else error_board
                             nextPoint = nextEmpty b n
                             nextPointAsNumber = xy2n b nextPoint
                             nextPointCExists = nextEmpty bC nextPointAsNumber /= (-1,-1)
                             nextPointMExists = nextEmpty bM nextPointAsNumber /= (-1,-1)
                             result_mark = if isBoardWithError bM
                                           then error_board
                                           else if isBoardComplete bM
                                            then bM
                                            else bM'
                             error_board = b { mapa = [[]] }

  findSolutionImpl' :: Board -> Int -> Int -> Board                        
  findSolutionImpl' b n i | i == 13 = b
                          | isBoardWithError bC = result_mark
                          | isBoardComplete bC  = bC
                          | otherwise           = if (mapa bC') == mapa (error_board)
                                                then result_mark
                                                else bC'
                      where bC = runHeuristics (putFieldAt b nextPoint Gas)
                            bC' = if nextPointCExists then (findSolutionImpl' bC nextPointAsNumber (i+1)) else error_board 
                            bM = runHeuristics (putFieldAt b nextPoint None)
                            bM' = if nextPointMExists then (findSolutionImpl' bM nextPointAsNumber (i+1)) else error_board
                            nextPoint = nextEmpty b n
                            nextPointAsNumber = xy2n b nextPoint
                            nextPointCExists = nextEmpty bC nextPointAsNumber /= (-1,-1)
                            nextPointMExists = nextEmpty bM nextPointAsNumber /= (-1,-1)
                            result_mark = if isBoardWithError bM
                                          then error_board
                                          else if isBoardComplete bM
                                            then bM
                                            else bM'
                            error_board = b { mapa = [[]] }


  
  runHeuristics :: Board -> Board
  runHeuristics board = finalBoard where
    board1 = filterEmptyRowsColumns board (0,0)
    board2 = fillMatchingFields board1 (0,0)
    finalBoard = board2

  filterDistantFields :: Board -> Point -> Board
  filterDistantFields board (colIdx,rowIdx)
    | rowIdx < length r && colIdx < length c = finalBoard
    | rowIdx < length r = filterDistantFields board (0,rowIdx+1)
    | otherwise = board
    where
      m = mapa board
      r = rows board
      c = cols board
      updatedBoard = putFieldAt board (colIdx,rowIdx) field
      finalBoard = filterDistantFields updatedBoard (colIdx+1,rowIdx)
      field
        | getFieldTypeAt board (colIdx,rowIdx) == None
          && (getFieldTypeAt board (colIdx,rowIdx-1) == House
            || getFieldTypeAt board (colIdx-1,rowIdx) == House
            || getFieldTypeAt board (colIdx+1,rowIdx) == House
            || getFieldTypeAt board (colIdx,rowIdx+1) == House) == False = Empty
        | otherwise = (m !! rowIdx) !! colIdx

  fillMatchingFields :: Board -> Point -> Board
  fillMatchingFields board (colIdx,rowIdx)
    | rowIdx < length r = finalRowBoard
    | colIdx < length c = finalColBoard
    | otherwise = board
    where
      r = rows board
      c = cols board
      finalRowBoard = fillMatchingFields updatedRowBoard (colIdx,rowIdx+1)
      rowCount = countSthInRow board (colIdx,rowIdx) None
      updatedRowBoard = if (rowCount /= 0 && rowCount == (r !! rowIdx))
                        then pushGasInRow board (0,rowIdx)
                        else board
      finalColBoard = fillMatchingFields updatedColBoard (colIdx+1,rowIdx)
      colCount = countSthInCol board (colIdx,rowIdx) None
      updatedColBoard = if (colCount /= 0 && colCount == (c !! colIdx))
                        then pushGasInCol board (colIdx,0)
                        else board

  pushGasInRow :: Board -> Point -> Board
  pushGasInRow board (colIdx,rowIdx)
    | colIdx < length c = finalBoard
    | otherwise = board
    where
      c = cols board
      finalBoard = pushGasInRow updatedBoard (colIdx+1,rowIdx)
      updatedBoard = if getFieldTypeAt board (colIdx,rowIdx) == None
                     then filterGasTankProximity boardGas (colIdx,rowIdx)
                     else board
      boardGas = putFieldAt board (colIdx,rowIdx) Gas

  pushGasInCol :: Board -> Point -> Board
  pushGasInCol board (colIdx,rowIdx)
    | rowIdx < length r = finalBoard
    | otherwise = board
    where
      r = rows board
      finalBoard = pushGasInCol updatedBoard (colIdx,rowIdx+1)
      updatedBoard = if getFieldTypeAt board (colIdx,rowIdx) == None
                      then filterGasTankProximity boardGas (colIdx,rowIdx)
                      else board
      boardGas = putFieldAt board (colIdx,rowIdx) Gas

  filterGasTankProximity :: Board -> Point -> Board
  filterGasTankProximity board (colIdx,rowIdx) = finalBoard where
    boardNW = filterGasTankProximityImpl board (colIdx-1,rowIdx-1)
    boardN = filterGasTankProximityImpl boardNW (colIdx,rowIdx-1)
    boardNE = filterGasTankProximityImpl boardN (colIdx+1,rowIdx-1)
    boardE = filterGasTankProximityImpl boardNE (colIdx+1,rowIdx)
    boardSE = filterGasTankProximityImpl boardE (colIdx+1,rowIdx+1)
    boardS = filterGasTankProximityImpl boardSE (colIdx,rowIdx+1)
    boardSW = filterGasTankProximityImpl boardS (colIdx-1,rowIdx+1)
    boardW = filterGasTankProximityImpl boardSW (colIdx-1,rowIdx)
    finalBoard = boardW

  filterGasTankProximityImpl :: Board -> Point -> Board
  filterGasTankProximityImpl board (colIdx,rowIdx) = finalBoard where
    isNone = (getFieldTypeAt board (colIdx,rowIdx) == None)
    finalBoard = if isNone
              then putFieldAt board (colIdx,rowIdx) Empty
              else board
