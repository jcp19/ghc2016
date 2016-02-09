import Data.List(maximumBy)
import Data.Function(on)

type Canvas = [[Char]]
type Position = (Int,Int)

{- Arguments: canvas -> (row, column, s)
   Returns: canvas with '#' in positions [(x,y) | x <- [row-s..row+s], y <- [column-s..column+s]] 

   Paint all cells inside a square of (2*s + 1) x (2*s + 1) dimensions centered at (row,column) 
-}

paintSquare :: Canvas -> (Int,Int,Int) -> Canvas
paintSquare [] _ = error "Empty Canvas"
paintSquare canvas (r,c,s) = let positions = [(x,y) | x <- [r-s..r+s], y <- [c-s..c+s]]
                                 result = foldr (\pos canvas' -> insertPosCanvas canvas' pos '#') canvas positions
                             in result

{- Arguments: canvas
   Returns: ((r1,c1),(r2,c2))

   Compute the row segment with the greatest number of cells that need to be painted.
-}

bestRow :: Canvas -> (Position,Position)
bestRow canvas = snd $ bestRowSeg
    where bestRowSeg = maximumBy (compare `on` (length . fst)) (potentialRowSegs canvas (0,0))

{- 
   Compute all the row segments that need to be painted and their starting and ending position.
-}

potentialRowSegs :: Canvas -> Position -> [(String, (Position, Position))]
potentialRowSegs [] _ = []
potentialRowSegs ([]:t) (x,y) = potentialRowSegs t (x+1, 0)
potentialRowSegs l@(h:t) (x,y) | hashtags == []     = potentialRowSegs t (x+1, 0)
                               | otherwise          = (hashtags, ((x,yStart),(x,yEnd))) : (potentialRowSegs (rest':t) (x,yEnd+1)) 
    where (dots, rest) = span (=='.') h
          (hashtags, rest') = span (=='#') rest
          yStart = y + length dots
          yEnd = yStart + length hashtags - 1


insertPosCanvas :: Canvas -> Position -> Char -> Canvas
insertPosCanvas canvas (x,y) char = (take x canvas) ++ [take y (canvas!!x) ++ [char] ++ drop (y+1) (canvas!!x)] ++ (drop (x+1) canvas)
