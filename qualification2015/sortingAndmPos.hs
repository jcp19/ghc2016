import Data.List(minimumBy,sortBy)
import Data.Function(on)

data Server = Vazio | Ocupado | Serv Int Int Int Int Int Int deriving (Show, Read, Eq)
type DataCenter = [[Server]]

ratio :: Server -> Float
ratio Vazio = error "Invalid argument"
ratio Ocupado = error "Invalid argument"
ratio (Serv _ cap size _ _ _) = (fromIntegral cap) / (fromIntegral size)

sortByDescending :: (a -> a -> Ordering) -> ([a] -> [a])
sortByDescending cmp = sortBy (flip cmp)

sortRatios :: [Server] -> [Server]
sortRatios servers = sortByDescending (compare `on` ratio) servers

rowCapacity :: [Server] -> Int
rowCapacity [] = 0
rowCapacity ((Serv _ cap _ _ _ _):t) = cap + rowCapacity t
rowCapacity (h:t) = rowCapacity t

sortCapacities :: DataCenter -> DataCenter
sortCapacities dc = sortByDescending (compare `on` rowCapacity) dc

melhor_pos :: [Server] -> Server -> Int
melhor_pos [] _ = error "Empty list"
melhor_pos l serv = snd (minimumBy (compare `on` fst) (possibleSlots l serv 0))

-- returns [(number free consecutive slots, start of segment)]
possibleSlots :: [Server] -> Server -> Int -> [(Int,Int)] 
possibleSlots [] _ _ = []
possibleSlots l s@(Serv _ _ size _ _ _) start | nFree >= size         = (nFree,start):(possibleSlots remaining s start')
                                              | otherwise             = possibleSlots remaining s start'
    where nFree = length (takeWhile (==Vazio) l)
          (occupied, remaining) = span (/= Vazio) (drop nFree l)
          start' = start + nFree + (length occupied)