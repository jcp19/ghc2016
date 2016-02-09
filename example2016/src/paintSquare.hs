type Canvas = [[Char]]


{-Parameters: canvas -> (row, column, s)
  Returns: canvas with '#' in positions [(x,y) | x <- [row-s..row+s], y <- [column-s..column+s]] -}

paintSquare :: Canvas -> (Int,Int,Int) -> Canvas
paintSquare [] _ = error "Empty Canvas"
paintSquare canvas (r,c,s) = let positions = [(x,y) | x <- [r-s..r+s], y <- [c-s..c+s]]
                                 result = foldr (\(x,y) canvas' -> insertPosCanvas canvas' x y '#') canvas positions
                             in result

insertPosCanvas :: Canvas -> Row -> Column -> Char -> Canvas
insertPosCanvas canvas x y char = (take x canvas) ++ [take y (canvas!!x) ++ [char] ++ drop (y+1) (canvas!!x)] ++ (drop (x+1) canvas)