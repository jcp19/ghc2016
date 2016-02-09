vAux :: Int -> Int -> Int -> [[Char]] -> [[Char]]
vAux c x y (h:t) | c > 0 = ([(take y h)++['#']++(drop (y+1) h)])++(vAux (c-1) x y t)
				 | otherwise = []

horizontal :: Int -> Int -> Int -> [[Char]] -> [[Char]]
horizontal comp x y m = (take x m) ++ [take y (m!!x) ++ replicate (comp) '#' ++ (drop (y+comp) (m!!x))] ++ (drop (x+1) m)

vertical :: Int -> Int -> Int -> [[Char]] -> [[Char]]
vertical c x y m = (take x m) ++ (vAux c x y m) ++ (drop (x+(c)) m)

paint_line :: Int -> Int -> Int -> Int -> [[Char]] -> [[Char]]
paint_line x1 y1 x2 y2 m | x1 == x2 && y1 == y2  = m --nope
                         | x1 == x2              = horizontal (abs(y1-y2)) x1 (minimum [y1, y2]) m
						 | y1 == y2				 = vertical (abs(x1-x2)) (minimum [x1,x2]) y2 m
						 | otherwise			 = m --leave it alone


