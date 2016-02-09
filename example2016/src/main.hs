module Main where

import Data.List
import Data.Function

type Canvas = [[Char]]

newCanvas :: Int -> Int -> Canvas
newCanvas x y = replicate x (replicate y '.')

insertPosCanvas :: Canvas -> Int -> Int -> Char -> Canvas
insertPosCanvas canvas x y char = (take x canvas) ++ [take y (canvas!!x) ++ [char] ++ drop (y+1) (canvas!!x)] ++ (drop (x+1) canvas)

eraseCell :: Canvas -> Int -> Int -> Canvas
eraseCell canvas x y = insertPosCanvas canvas x y '.'

readCanvas :: IO Canvas
readCanvas = do board <- getContents
                return$lines board

main = do dims <- getLine
          dims <- return ((map (read) (words dims)) :: [Int])
          (rows, columns) <- return (dims!!0, dims!!1)
          canvas <- readCanvas
          print canvas
          

