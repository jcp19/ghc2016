import Data.List(sortBy)
import Data.Function(on)

data Server = Vazio | Ocupado | Serv Int Int Int deriving (Show, Read, Eq)
type DataCenter = [[Server]]

ratio :: Server -> Float
ratio Vazio = error "Invalid argument"
ratio Ocupado = error "Invalid argument"
ratio (Serv _ cap size) = (fromIntegral cap) / (fromIntegral size)

sortByDescending :: (a -> a -> Ordering) -> ([a] -> [a])
sortByDescending cmp = sortBy (flip cmp)

sortRatios :: [Server] -> [Server]
sortRatios servers = sortByDescending (compare `on` ratio) servers

rowCapacity :: [Server] -> Int
rowCapacity [] = 0
rowCapacity ((Serv _ cap _):t) = cap + rowCapacity t
rowCapacity (h:t) = rowCapacity t

sortCapacities :: DataCenter -> DataCenter
sortCapacities dc = sortByDescending (compare `on` rowCapacity) dc 