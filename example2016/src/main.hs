module Main where

import Data.List
import Data.Function

type Canvas = [[Char]]
type Position = (Int, Int)

emptyCanvas :: Int -> Int -> Canvas
emptyCanvas x y = replicate x (replicate y '.')

insertPosCanvas :: Canvas -> Position -> Char -> Canvas
insertPosCanvas canvas (x,y) char = (take x canvas) ++ [take y (canvas!!x) ++ [char] ++ drop (y+1) (canvas!!x)] ++ (drop (x+1) canvas)

{-
eraseCell :: Canvas -> Position -> Canvas
eraseCell canvas (x,y) = insertPosCanvas canvas (x,y) '.'
-}

readCanvas :: IO Canvas
readCanvas = do board <- getContents
                return$lines board

bestColumn :: Canvas -> (Position, Position)
-- computes the best column in which we can paint a line, the output is in the same format as the input for the PAINT_LINE
bestColumn a = invert (bestRow$transpose a) 
                where invert = \((a,b), (c,d)) -> ((b,a), (d,c)) 

paintLine :: Canvas -> Char -> (Position, Position) -> Canvas
paintLine canvas c ((x,y), (a,b)) | x > a || y > b = canvas
                                  | x == a = paintLine (insertPosCanvas canvas (x,y) c) ((x,y+1), (a,b))
                                  | y == b = paintLine (insertPosCanvas canvas (x,y) c) ((x+1,y), (a,b))
                                  | otherwise = error "You Fucked UP"

paintItAux :: Canvas -> Canvas -> [String] -> (Canvas, [String])
-- given the original canvas and an empty "history" of Comands, it computes de functions needed. this function should be connected to one of type Canvas -> [String] that returns only the history
-- an empty canvas must be passed as a 2nd argument for simplicity's sake
paintItAux canvas empty hist | canvas == empty = ([], hist)
                             | helpY bc >= helpX br = (paintLine canvas '.' bc, hist ++ (newRec bc))
                             | otherwise = (paintLine canvas '.' br, hist ++ (newRec br))
                                 where
                                 	br = bestRow canvas
                                    bc = bestColumn canvas
                                    helpY = (\((a,b), (c,d)) -> c - a)
                                    helpX = (\((a,b), (c,d)) -> d - b)
                                    newRec ((a,b),(c,d)) = ["PAINT_LINE " ++ show a++ " " ++ show b ++ " " ++ show c ++ " " ++ show d]

paintIt :: Canvas -> [String]
paintIt canvas | length canvas == 0 = []
               | otherwise = snd(paintItAux canvas (emptyCanvas (length canvas) (length (head canvas))) []) 

main = do dims <- getLine
          dims <- return ((map (read) (words dims)) :: [Int])
          (rows, columns) <- return (dims!!0, dims!!1)
          canvas <- readCanvas
          print (paintIt canvas)
          