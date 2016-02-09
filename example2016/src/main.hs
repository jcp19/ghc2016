module Main where

import Data.List
import Data.Function

type Canvas = [[Char]]
type Position = ((Int, Int), (Int, Int))

newCanvas :: Int -> Int -> Canvas
newCanvas x y = replicate x (replicate y '.')

insertPosCanvas :: Canvas -> Position -> Char -> Canvas
insertPosCanvas canvas (x,y) char = (take x canvas) ++ [take y (canvas!!x) ++ [char] ++ drop (y+1) (canvas!!x)] ++ (drop (x+1) canvas)

eraseCell :: Canvas -> Position -> Canvas
eraseCell canvas (x,y) = insertPosCanvas canvas (x,y) '.'

readCanvas :: IO Canvas
readCanvas = do board <- getContents
                return$lines board

bestColumn :: Canvas -> (Position, Position)
-- computes the best column in which we can paint a line, the output is in the same format as the input for the PAINT_LINE
bestColumn = invert$transpose$bestLine
                where invert = \((a,b), (c,d)) -> ((b,a), (d,c)) 

paintLine :: Canvas -> (Position, Position) -> Canvas
paintLine canvas ((x,y), (a,b)) | x > a || y > b = canvas
                                | x == a = paintLine (insertPosCanvas canvas (x,y) '#') ((x,y+1), (a,b))
                                | y == b = paintLine (insertPosCanvas canvas (x,y) '#') ((x+1,y), (a,b))
                                | otherwise = error "You Fucked UP"

main = do dims <- getLine
          dims <- return ((map (read) (words dims)) :: [Int])
          (rows, columns) <- return (dims!!0, dims!!1)
          canvas <- readCanvas
          print canvas
          

