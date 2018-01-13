module Spop where
  import Board
  import BoardGUI
  import Solver

  import System.Directory
  import System.IO.Error
  import Control.Exception

  main = do
    putStrLn "\nŁamigłówka architekta.\n"
    askForFile

  askForFile = do
    putStrLn "Podaj nazwę pliku wejściowego z łamigłówką:"
    -- TODO uncomment
    -- fileName <- getLine
    let fileName = "q2.txt"
    putStrLn "DEBUG: Nazwa pliku zaladowana automatycznie"
    fileOK <- doesFileExist fileName

    if (fileOK)
      then do
        myData <- readFile fileName
        putStrLn "DEBUG: Zawartosc pliku:"
        putStrLn myData

        let fileLine = lines myData
        let row = read (fileLine !! 0) :: [Int]
        let col = read (fileLine !! 1) :: [Int]
        let house = read (fileLine !! 2) :: [(Int, Int)]

        let map = generateMap row col house
        let board = Board map row col house row col
        putStrLn "Wczytana plansza:"
        drawBoard board

        putStrLn "Rozwiazuje zadanie:"
        let solvedBoard = solve board

        putStrLn "DEBUG: Rozwiazana plansza:"
        drawBoard solvedBoard

        putStrLn "Rozwiazana plansza:"
        let solvedBoardFinal = solvedBoard {rows = (rowsBase solvedBoard), cols = (colsBase solvedBoard)}
        drawBoard solvedBoardFinal

        -- save output
      else do
        putStrLn "Nie znaleziono podanego pliku."
        askForFile

  wrongFormat :: SomeException -> IO ()
  wrongFormat _ = do
    putStrLn "Podany plik ma zły format!"
    askForFile