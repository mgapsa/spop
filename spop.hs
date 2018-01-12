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
    let fileName = "q1.txt"
    putStrLn "DEBUG: Nazwa pliku zaladowana automatycznie"
    fileOK <- doesFileExist fileName

    if (fileOK)
      then do
        myData <- readFile fileName
        putStrLn "DEBUG: Zawartosc pliku:"
        putStrLn myData

        let fileLine = lines myData
        let rows = read (fileLine !! 0) :: [Int]
        let cols = read (fileLine !! 1) :: [Int]
        let houses = read (fileLine !! 2) :: [(Int, Int)]

        let map = getMap rows cols houses
        let board = Board map rows cols houses
        putStrLn "Wczytana plansza:"
        drawMap board

        putStrLn "Rozwiazuje zadanie:"
        let solvedBoard = solve board

        putStrLn "Rozwiazana plansza:"
        drawMap solvedBoard

        -- save output
      else do
        putStrLn "Nie znaleziono podanego pliku."
        askForFile

  wrongFormat :: SomeException -> IO ()
  wrongFormat _ = do
    putStrLn "Podany plik ma zły format!"
    askForFile