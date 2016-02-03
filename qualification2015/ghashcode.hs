module Main where

import Data.List
import Data.Function

data Server = Vazio | Ocupado | Serv Int Int Int Int Int Int deriving (Show, Read, Eq, Ord)
-- id capacity size x y pool
type DataCenter = [[Server]]
type Pool = [Int]
--em cada indice indica que capacidade da pool está na row com esse indice

newDataCenter :: Int -> Int -> DataCenter
newDataCenter x y = replicate x (replicate y Vazio)

-- insere na posição escolhida do DC, nao verifica se é possivel inserir na posiçao escolhida, isso deve ser verificado antes
insertPosDataCenter :: DataCenter -> Int -> Int -> Server -> DataCenter
insertPosDataCenter dc x y a@(Serv id cap size _ _ pool) = (take x dc) ++ [take y (dc!!x) ++ [(Serv id cap size x y pool)] ++ (replicate (size-1) Ocupado) ++ drop (y+size) (dc!!x)] ++ (drop (x+1) dc)
insertPosDataCenter dc x y a = (take x dc) ++ [take y (dc!!x) ++ [a] ++ drop (y+1) (dc!!x)] ++ (drop (x+1) dc)

insereOcupados :: Int -> DataCenter -> IO DataCenter
-- quantos_ocupados -> lista
insereOcupados 0 dc = return dc
insereOcupados x dc = do pos <- getLine
                         vars <- return ((map (read) (words pos)) :: [Int])
                         newDc <- return (insertPosDataCenter dc (vars!!0) (vars!!1) Ocupado)
                         newDc <- insereOcupados (x-1) newDc
                         return newDc

leServers :: Int -> [String] -> [Server]
leServers _ [] = []
leServers x (h:t) = (Serv x (vars!!1) (vars!!0) (-1) (-1) (-1)) : (leServers (x+1) t)
                   where
                        vars = (map read (words h)) :: [Int]

indexer :: [a] -> [(a,Int)]
indexer x = indexerAux x 0
              where indexerAux [] _ = []
                    indexerAux (h:t) x = ((h,x):(indexerAux t (x+1)))

capacity :: [Server] -> Int
capacity [] = 0
capacity ((Serv _ cap _ _ _ _):t) = cap + (capacity t)
capacity (x:xs) = capacity xs

linha_menorCap_ondeCabe :: DataCenter -> Server -> Int
linha_menorCap_ondeCabe dc (Serv _ _ size _ _ _) | (length linhas_candidatas) /= 0 =  snd (head linhas_candidatas)
                                                 | otherwise = (-1)
                                                       where
                                                           linhas_candidatas = sort (map (\(x,y) -> (capacity x, y)) (filter (\(x,y) -> maxSlotFree x >= size) (indexer dc)))

maxSlotFree :: [Server] -> Int
maxSlotFree [] = 0
maxSlotFree k = max (length (takeWhile (\x -> x == Vazio) k)) (maxSlotFree (dropWhile (\x -> x /= Vazio) (dropWhile (\x -> x == Vazio) k)) )

fillDataCenter :: DataCenter -> [Server] -> DataCenter
-- dado o data center, enche-o com os servidores (a lista de Servers está ordenada decrescentemente por racio)
-- é preciso limitar o numero de elementos que se inserem na pool!! (isto é, só se pretende inserir um numero definido de servidores)
fillDataCenter dc [] = dc
fillDataCenter dc (h:t) | linha_menorCap_ondeCabe dc h == -1 = fillDataCenter dc t
                        | otherwise = fillDataCenter (insertPosDataCenter dc (linha_menorCap_ondeCabe dc h) (melhor_pos (dc!!(linha_menorCap_ondeCabe dc h)) h) h) t

createPools :: Int -> Int -> [Pool]
createPools npools nrows = replicate npools (replicate nrows 0)

distribui :: [Pool] -> DataCenter -> Int-> Int -> Int-> Int -> (DataCenter, [Pool])
-- pool, dc, coordenadas a consultar agora, coordenadas maximas
distribui pool dc x y xmax ymax | y > ymax = (dc,pool)
                                | x > xmax = distribui pool dc 0 (y+1) xmax ymax
                                | servidorAtual == Ocupado || servidorAtual == Vazio = distribui pool dc (x+1) y xmax ymax 
                                | otherwise = distribui poolsAtualizada (insertPosDataCenter dc x y (atualizaPool servidorAtual poolMenorCapG)) (x+1) y xmax ymax 
                                         where 
                                           servidorAtual = ((dc !! x) !! y)
                                           (poolsAtualizada, poolMenorCapG) = calculaPool pool x (capacidadeServ servidorAtual)

--calculaPool :: [Pool] -> Int -> Int -> ([Pool], Int)
--calculaPool pools row cap =

capacidadeServ :: Server -> Int
capacidadeServ (Serv _ cap _ _ _ _) = cap
capacidadeServ a = 0

atualizaPool :: Server -> Int -> Server                                  
atualizaPool (Serv id capacity size x y _) pool = (Serv id capacity size x y pool) 

printResposta :: DataCenter -> IO()
printResposta dc = formatedPrint (sort (filter (\x -> x /= Ocupado && x /= Vazio) (concat dc))) 0 

formatedPrint :: [Server] -> Int -> IO()
formatedPrint [] _  = putStr ""
formatedPrint a@((Serv id capacity size x y pool):t) index | id == index = do putStrLn(unwords([show x, show y, show pool]))
                                                                              formatedPrint t (index+1)
                                                           | otherwise = do putStrLn "x"
                                                                            formatedPrint a (index + 1)

main = do primeiraLinha <- getLine
          vars <- return ((map (read) (words primeiraLinha)) :: [Int])
          dc <- return (newDataCenter (vars!!0) (vars!!1))
          dc <- insereOcupados (vars!!2) dc
          servers <- getContents
          servers <- return (sortRatios(leServers 0 (lines servers)))
          
          dc <- return (fillDataCenter dc servers)
          --print dc
          pools <- return (createPools (vars!!3) (vars!!0))
          -- a distribui vai dar um tuplo "resposta" com (pools, servers)
          resposta <- return (distribui pools dc 0 0 ((vars!!0) -1) ((vars!!1) -1)) 
          print (snd (resposta))
          --a lista final de servidores vai ter a localização dos servidores nas pools
          --  printResposta (fst(resposta))
          -- print capacidade_minima_garantida


calculaPool :: [Pool] -> Int -> Int -> ([Pool], Int)
calculaPool pools row cap = ( addPool pools poolNova row cap , poolNova)
                                where
                                   poolNova = minIndex ((map (\x -> x!!row)) pools)

addPool :: [Pool] -> Int -> Int -> Int -> [Pool]
addPool pools poolAtualizar row cap = (take poolAtualizar pools ) ++ [novaPool] ++ (drop (poolAtualizar+1) pools )
                                       where 
                                         novaPool = (take row (pools!!poolAtualizar)) ++ [cap + ((pools!!poolAtualizar)!!row)] ++ (drop (row+1) (pools!!poolAtualizar)) 


minIndex :: Ord a => [a] -> Int
minIndex list = snd . minimum $ zip list [0 .. ]

-- JM


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