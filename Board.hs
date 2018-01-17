module Board where
  -- Point = (Column, Row)
  type Point = (Int, Int)

  -- typy pol znajdujace sie w zagadce
  data FieldType = None | Empty | House | Gas deriving Eq
  type MapType = [[FieldType]]

  -- struktura danych przechowujaca dane o zagadce
  data Board = Board {mapa::MapType,
                      rows::[Int],
                      cols::[Int],
                      houses::[Point],
                      rowsBase::[Int],
                      colsBase::[Int]} deriving (Eq)

  -- generuje poczatkowa mape na podstawie list
  -- zawierajacych informacje o polozeniach domow
  generateMap :: [Int] -> [Int] -> [Point] -> MapType
  generateMap rows cols houses = mapa where
    xSize = length cols
    ySize = length rows
    mapa = generateRow houses 0 (xSize, ySize)

  -- generuje mape wiersz po wierszu
  generateRow :: [Point] -> Int -> Point -> MapType
  generateRow houses rowIdx (xSize, ySize)
    | rowIdx < ySize = row : generateRow houses (rowIdx+1) (xSize, ySize)
    | otherwise = [[]]
    where
      row = generateElement houses rowIdx 0 (xSize, ySize)

  -- generuje elementy w mapie element po elemencie w danym wierszu
  generateElement :: [Point] -> Int -> Int -> Point -> [FieldType]
  generateElement houses rowIdx colIdx (xSize, ySize)
    | colIdx < xSize = house : generateElement houses rowIdx (colIdx+1) (xSize, ySize)
    | otherwise = []
    where
      house = isHouse houses (rowIdx,colIdx)

  -- sprawdza w liscie zawirajacej domy
  -- czy na polo o zadanych wspolrzednych stoi dom
  isHouse :: [Point] -> Point -> FieldType
  isHouse [] _ = None
  isHouse (h:hs) housePosition
    | h == housePosition = House
    | otherwise = isHouse hs housePosition

  -- sprawdza istniejacy typ pola odczytujac aktualny stan zagadki
  getFieldTypeAt :: Board -> Point -> FieldType
  getFieldTypeAt board (colIdx, rowIdx)
    | rowIdx < 0 || rowIdx >= length r || colIdx < 0 || colIdx >= length c = None
    | otherwise = (m !! rowIdx) !! colIdx
    where
      m = mapa board
      r = rows board
      c = cols board

  -- zmienia typ pola w zagadce, nadpisuje istniejace pole nowym typem
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

  -- dekrementuje wartosc elementu znajdujacego sie
  -- na liscie jako element o zadanym indeksie
  decrementElementInList :: [Int] -> Int -> [Int]
  decrementElementInList [] _ = []
  decrementElementInList (x:xs) idx
    | idx > 0 = x : decrementElementInList xs (idx-1)
    | otherwise = (x-1) : xs

  -- oznacza pola sasiadujace ze zbiornikiem z gazem jako pola puste
  -- nie mozna postawic innego zbiornika z gazem
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

  -- implementuje filterGasTankProximity
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

  -- zmienia wartosc zadanego pola na mapie na pole o zadanym typie
  setMap :: MapType -> Point -> FieldType -> [Int] -> [Int] -> MapType
  setMap mapa (dstColIdx,dstRowIdx) field rows cols = newBoard where
    colSize = length cols
    rowSize = length rows
    newBoard = setRow mapa 0 (colSize,rowSize) (dstColIdx,dstRowIdx) field

  -- implementuje setMap, sprawdzajac wiersz po wierszu
  setRow :: MapType -> Int -> Point -> Point -> FieldType -> MapType
  setRow mapa rowIdx (colSize,rowSize) (dstColIdx,dstRowIdx) field
    | rowIdx < rowSize = row : setRow mapa (rowIdx+1) (colSize,rowSize) (dstColIdx,dstRowIdx) field
    | otherwise = [[]]
    where
      row = setElement mapa 0 rowIdx (colSize,rowSize) (dstColIdx,dstRowIdx) field

  -- implememtuje setRow, sprawdzajac element po elemencie
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

  -- sprawdza czy zadane pole jest polem jeszcze nie zaklasyfikowanym
  isEmptyAt :: Board -> Point -> Bool
  isEmptyAt board point = getFieldTypeAt board point == None


  -- liczy elementy okreslonego typu w zadanym wierszu
  countSthInRow :: Board -> Point -> FieldType -> Int
  countSthInRow board (colIdx,rowIdx) filed
    | colIdx < length c = val + countSthInRow board (colIdx+1,rowIdx) filed
    | otherwise = 0
    where
      c = cols board
      val | getFieldTypeAt board (colIdx,rowIdx) == filed = 1
          | otherwise = 0

  -- liczy elementy okreslonego typu w zadanej kolumnie
  countSthInCol :: Board -> Point -> FieldType -> Int
  countSthInCol board (colIdx,rowIdx) field
    | rowIdx < length r = val + countSthInCol board (colIdx,rowIdx+1) field
    | otherwise = 0
    where
      r = rows board
      val | getFieldTypeAt board (colIdx,rowIdx) == field = 1
          | otherwise = 0

  -- liczy elemety okreslonego typu w liscie
  countSthInList :: [FieldType] -> FieldType -> Int
  countSthInList [] _ = 0
  countSthInList (x:xs) field = val + countSthInList xs field where
    val = if x == field
          then 1
          else 0

  -- sprawdza czy dany wiersz jest juz wypelniony,
  -- czy w danym wierszu wstawiono juz wszsytkie wymagane zbiorniki z gazem
  areRowsComplete :: Board -> Int -> Bool
  areRowsComplete board n | n == length(rows board) = True
                          | otherwise = ((rows board) !! n) == 0 && areRowsComplete board (n+1)
  -- sprawdza czy dany wiersz jest juz wypelniony,
  -- czy w danej kolumnie wstawiono juz wszsytkie wymagane zbiorniki z gazem
  areColsComplete :: Board -> Int -> Bool
  areColsComplete board n | n == length(cols board) = True
                          | otherwise = ((cols board) !! n) == 0 && areColsComplete board (n+1)

  -- sprawdza czy zagadka zostala juz rozwiazana,
  -- sprawdza czy wiersze i kolumny uzupelnione sa prawidlowo
  isBoardComplete :: Board -> Bool
  isBoardComplete board = (areRowsComplete board 0) && (areColsComplete board 0) && (isHouseWithGas board (0,0))

  -- sprawdza czy dom ma dostep do zbiornika z gazem
  isHouseWithGas :: Board -> Point -> Bool
  isHouseWithGas board (colIdx,rowIdx)
    | rowIdx < length r && colIdx < length c = finalStatus && isHouseWithGas board (colIdx+1,rowIdx)
    | rowIdx < length r = isHouseWithGas board (0,rowIdx+1)
    | otherwise = True
    where
      r = rows board
      c = cols board
      finalStatus = if getFieldTypeAt board (colIdx,rowIdx) == House
                    then isHouseWithGasImpl board (colIdx,rowIdx)
                    else True

  -- implementuje isHouseWithGas
  isHouseWithGasImpl :: Board -> Point -> Bool
  isHouseWithGasImpl board (colIdx,rowIdx) = finalStatus where
    fieldN = getFieldTypeAt board (colIdx,rowIdx-1)
    fieldE = getFieldTypeAt board (colIdx+1,rowIdx)
    fieldS = getFieldTypeAt board (colIdx,rowIdx+1)
    fieldW = getFieldTypeAt board (colIdx-1,rowIdx)
    neightbours = [fieldN, fieldE, fieldS, fieldW]
    finalStatus = if countSthInList neightbours Gas > 0
                  then True
                  else False

  -- sprawdza czy dom moze miec podlaczony zbiornik z gazem
  -- pozawla okreslic sytuacje gdy doszlo do blokady
  canHouseHaveGas :: Board -> Point -> Bool
  canHouseHaveGas board (colIdx,rowIdx)
    | rowIdx < length r && colIdx < length c = finalStatus && canHouseHaveGas board (colIdx+1,rowIdx)
    | rowIdx < length r = canHouseHaveGas board (0,rowIdx+1)
    | otherwise = True
    where
      r = rows board
      c = cols board
      finalStatus = if getFieldTypeAt board (colIdx,rowIdx) == House
                    then canHouseHaveGasImpl board (colIdx,rowIdx)
                    else True

  canHouseHaveGasImpl :: Board -> Point -> Bool
  canHouseHaveGasImpl board (colIdx,rowIdx) = finalStatus where
    fieldNX = getFieldTypeAt board (colIdx,rowIdx-1)
    fieldEX = getFieldTypeAt board (colIdx+1,rowIdx)
    fieldSX = getFieldTypeAt board (colIdx,rowIdx+1)
    fieldWX = getFieldTypeAt board (colIdx-1,rowIdx)
    neightbours = [fieldNX,fieldEX, fieldSX, fieldWX]
    finalStatus = if countSthInList neightbours Gas > 0 || countSthInList neightbours None > 0
                  then True
                  else False

  -- sprawdza czy wystepuje blad w danym wierszu
  areRowsWithError :: Board -> Int -> Bool
  areRowsWithError board n | n == length(rows board) = False
                           | otherwise = ((countSthInRow board (0, n) Gas) > ((rowsBase board) !! n)) || areRowsWithError board (n+1)

  -- sprawdza czy wystepuje blad w danej kolumnie
  areColsWithError :: Board -> Int -> Bool
  areColsWithError board n | n == length(cols board) = False
                           | otherwise = (countSthInCol board (n, 0) Gas) > ((colsBase board) !! n) || areColsWithError board (n+1)

  -- sprawdza czy zagadka zawiera bledy
  isBoardWithError :: Board -> Bool
  isBoardWithError board = (areRowsWithError board 0) || (areColsWithError board 0) || not (canHouseHaveGas board (0,0))
