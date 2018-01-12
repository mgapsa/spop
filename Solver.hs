module Solver where
  import Board

  solve :: Board -> Board
  solve board = solvedBoard where
    -- map1 = filterDistantFields (mapa board) (0,0) (rows board) (cols board)
    -- updatedMap = runHeuristics map1 (rows board) (cols board) (houses board)
    -- solvedMap = (Board updatedMap (rows board) (cols board) (houses board))
    solvedBoard = board

  -- runHeuristics :: MapType -> [Int] -> [Int] -> [Point] -> MapType
  -- runHeuristics map0 rows cols houses = finalMap where
  --   map1 = filterEmptyRowsColumns map0 (0,0) rows cols
  --   finalMap = map1

  -- solveImpl :: MapType -> Point -> [Int] -> [Int] -> MapType
  -- solveImpl map (c , r) rows cols
  --   | r > (length rows -1) = map
  --   | r < (length rows -1) && c > (length cols -1) = solveImpl map (0, r+1) rows cols
  --   | (getFieldTypeAt map (c,r)) == House && c >= (length cols -1) = solveImpl (putGasNextToHouse map (c , r) rows cols) (0, r+1) rows cols
  --   | (getFieldTypeAt map (c,r)) == House && r >= (length rows -1) && c >= (length cols -1) = putGasNextToHouse map (c , r) rows cols
  --   | (getFieldTypeAt map (c,r)) == House = solveImpl (putGasNextToHouse map (c , r) rows cols) (c+1, r) rows cols
  --   | r >= (length rows -1) && c >= (length cols -1) = map
  --   | c >= (length cols -1) = solveImpl map (0, r+1) rows cols
  --   | otherwise = solveImpl map (c+1, r) rows cols

  -- --dodac sprawdzanie w jednej linii i jakies returny jak slepy zaulek
  -- putGasNextToHouse :: MapType -> Point -> [Int] -> [Int] -> MapType
  -- putGasNextToHouse map (c , r) rows cols = putFieldAt map (c , r) Gas rows cols

  -- filterEmptyRowsColumns :: MapType -> Point -> [Int] -> [Int] -> MapType
  -- filterEmptyRowsColumns map (colIdx, rowIdx) rows cols
  --   | rowIdx < length rows && colIdx < length cols = finalMap
  --   | rowIdx < length rows = filterEmptyRowsColumns map (0,rowIdx+1) rows cols
  --   | otherwise = map
  --   where
  --     updatedMap = putFieldAt map (colIdx,rowIdx) field rows cols
  --     finalMap = filterEmptyRowsColumns updatedMap (colIdx+1,rowIdx) rows cols
  --     field
  --       | ((rows !! rowIdx) == 0 || (cols !! colIdx) == 0) = Empty
  --       | otherwise = (map !! rowIdx) !! colIdx

  -- filterDistantFields :: MapType -> Point -> [Int] -> [Int] -> MapType
  -- filterDistantFields map (colIdx,rowIdx) rows cols
  --   | rowIdx < length rows && colIdx < length cols = finalMap
  --   | rowIdx < length rows = filterDistantFields map (0,rowIdx+1) rows cols
  --   | otherwise = map
  --   where
  --     updatedMap = putFieldAt map (colIdx,rowIdx) field rows cols
  --     finalMap = filterDistantFields updatedMap (colIdx+1,rowIdx) rows cols
  --     field
  --       | getFieldTypeAt map (colIdx,rowIdx) == None
  --         && (getFieldTypeAt map (colIdx,rowIdx-1) == House
  --           || getFieldTypeAt map (colIdx-1,rowIdx) == House
  --           || getFieldTypeAt map (colIdx+1,rowIdx) == House
  --           || getFieldTypeAt map (colIdx,rowIdx+1) == House) == False = Empty
  --       | otherwise = (map !! rowIdx) !! colIdx



  -- Przeszukuje wgłąb przestrzeń rozwiązań wstawiając w wolne pola zbiorniki
findSolution :: Board -> Board
findSolution b = if sn /= (-1, -1) then findSolutionImpl sb ((xy2n b sn)-1) else sb
                  where sb = runHeuristics b
                        sn = nextEmpty sb (-1)
findSolutionImpl :: Board -> Int -> Board                        
findSolutionImpl b n | board_problem bC  = result_mark
                     | board_complete bC = bC
                     | otherwise         = if (board bC') == board (error_board)
                                           then result_mark
                                           else bC'
                     where bC = solve (place_cont b next_xy) --PUT GAS NA MIEJSCE DANE
                           bC' = if next_n_C then (findSolutionImpl bC next_n) else error_board 
                           bM = solve (update_board b next_xy mark)
                           bM' = if next_n_M then (findSolutionImpl bM next_n) else error_board
                           next_n = next_empty b n
                           next_xy = n2xy b next_n
                           next_n_C = next_empty bC next_n /= (-1,-1)
                           next_n_M = next_empty bM next_n /= (-1,-1)
                           result_mark = if board_problem bM
                                         then error_board
                                         else if board_complete bM
                                          then bM
                                          else bM'
                           error_board = b { board = [[]] }


board_problemI BOARDS_COMPLETE