data State = Waiting Load | Loading Load | Delivery Position Load 

type Order = (ID, Position, Load)
type Wherehouse = (ID, Position, Load, [Drone])
type Drone = (ID, Position, State)

type ID = Int
type Product = Int
type Quantity = Int
type Position = (Int,Int)
type Load = [(Product, Quantity)]
