data State = Waiting Contents | Loading Contents | Delivery Position Contents 

type Order = (ID, Position, Contents)
type Wherehouse = (ID, Position, Contents, [Drone])
type Drone = (ID, Position, State)

type ID = Int
type Product = Int
type Quantity = Int
type Position = (Int,Int)
type Contents = [(Product, Quantity)]
type History = [String]