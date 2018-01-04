module Model where

type Position = (Int, Int)
data Board     = Board [Int] [Int] [Position] deriving (Show, Read)
    