module Spop where
    import System.Directory
    import System.IO.Error
    import Control.Exception
    
    main = do
      putStrLn "\nŁamigłówka architekta.\n"
      askForFile
          
    askForFile = do 
      putStrLn "Podaj nazwę pliku wejściowego z łamigłówką:"
      fileName <- getLine
      fileOK <- doesFileExist fileName
      if (fileOK)
        then do
          pls <- readFile fileName
          putStrLn pls
          putStrLn "ELO"
        else do
          putStrLn "Nie znaleziono podanego pliku."
          askForFile
          
    wrongFormat :: SomeException -> IO ()
    wrongFormat _ = do 
      putStrLn "Podany plik ma zły format!"
      askForFile