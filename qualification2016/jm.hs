data State = Waiting Contents | Loading Contents | Delivery Position Contents
    deriving (Eq,Ord,Show)

type Order = (ID, Position, Contents)
type Warehouse = (ID, Position, Contents, [Drone])
type Drone = (ID, Position, State)

type ID = Int
type Product = Int
type Quantity = Int
type Position = (Int,Int)
type Contents = [(Product, Quantity)]
type TurnCount = Int
type History = ([String], TurnCount)

distance :: Position -> Position -> Int
distance (x1,y1) (x2,y2) = let c1 = (x1 - x2) ^ 2
                               c2 = (y1 - y2) ^ 2
                           in (ceiling $ sqrt $ fromIntegral (c1 + c2))

filterByProximity :: [Order] -> Position -> Int -> [Order]
filterByProximity orders position maxDist = filter (\ (_,orderPos,_) -> distance orderPos position <= maxDist) orders

filterWaiting :: [Drone] -> [Drone]
filterWaiting [] = []
filterWaiting (drone@(_,_,Waiting _):remaining) = drone : filterWaiting remaining
filterWaiting (_:t) = filterWaiting t
