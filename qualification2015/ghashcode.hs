module Main where

import Data.List

data Server = Vazio | Ocupado | Serv Int Int Int Int Int Int deriving (Show, Read, Eq)
-- id capacity size x y pool
type DataCenter = [[Server]]
type Pool = [Int]
--em cada indice indica que capacidade da pool está na row com esse indice

newDataCenter :: Int -> Int -> DataCenter
newDataCenter x y = replicate x (replicate y Vazio)

-- insere na posição escolhida do DC, nao verifica se é possivel inserir na posiçao escolhida, isso deve ser verificado antes
insertPosDataCenter :: DataCenter -> Int -> Int -> Server -> DataCenter
insertPosDataCenter dc x y a@(Serv id cap size _ _ _) = (take x dc) ++ [take y (dc!!x) ++ [(Serv id cap size x y (-1))] ++ (replicate (size-1) Ocupado) ++ drop (y+size) (dc!!x)] ++ (drop (x+1) dc)
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

capacity :: [Servers] -> Int
capacity [] = 0
capacity ((Serv _ _ size _ _ _):t) = size + (capacity t)
capacity (x:xs) = capacity xs


linha_menorCap_ondeCabe :: DataCenter -> Server -> Int
linha_menorCap_ondeCabe dc (Serv _ _ size _ _ _) | length linhas_candidatas /= 0 =  snd (head linhas_candidatas)
                                                 | otherwise = -1
                                                    where
                                                      linhas_candidatas = sort (map (\((x,y) -> (capacity x, y))) (filter (\(x,y) -> maxSlotFree x >= size) (indexer dc)))

maxSlotFree :: [Servers] -> Int
maxSlotFree [] = 0
maxSlotFree k = max( (length (takeWhile (\x -> x == Vazio)) k), (maxSlotFree (DropWhile (\x -> x /= Vazio) (DropWhile (\x -> x == Vazio) k)) ))

fillDataCenter :: DataCenter -> [Servers] -> DataCenter
-- dado o data center, enche-o com os servidores (a lista de Servers está ordenada decrescentemente por racio)
-- é preciso limitar o numero de elementos que se inserem na pool!! (isto é, só se pretende inserir um numero definido de servidores)
fillDataCenter dc [] = dc
fillDataCenter dc (h:t) | linha_menorCap_ondeCabe dc h == -1 = fillDataCenter dc t
                        | otherwise = insertPosDataCenter dc (linha_menorCap_ondeCabe dc h) (melhor_pos (dc!!(linha_menorCap_ondeCabe dc h)) h) h

createPools :: Int -> Int -> [Pool]
createPools npools nrows = replicate npools (replicate nrows 0)

distribui :: [Pool] -> DataCenter -> Int-> Int -> Int-> Int -> (DataCenter, [Pool])
-- pool, dc, coordenadas a consultar agora, coordenadas maximas
distribui pool dc x y xmax ymax | y > ymax = (dc,pool)
                                | x > xmax = distribui pool dc 0 (y+1) xmax ymax
                                | ((dc !! x) !! y) == Ocupado || ((pool !! x) !! y) == Vazio = distribui pool dc (x+1) y xmax ymax 
                                | otherwise = distribui poolsAtualizada (insertPosDataCenter dc x y (atualizaPool ((dc !! x) !! y) poolMenorCapG)) (x+1) y xmax ymax 
                                         where (poolsAtualizada, poolMenorCapG) = calculaPool x (capacidade ((dc !! x) !! y))

atualizaPool :: Server -> Int -> Server                                  
atualizaPool (Serv id capacity size x y _) pool = (Serv id capacity size x y pool) 

printResposta :: DataCenter -> IO()
printResposta dc = formatedPrint (sort (filter (\x -> x /= Ocupado && x /= Vazio) (concat dc))) 0 

formatedPrint :: [Servers] -> Int -> IO()
formatedPrint [] _  = return
formatedPrint a@((Serv id capacity size x y pool):t) index | id == index = do println(unwords([show x, show y, show pool]))
                                                                            formatedPrint t (index+1)
                                                           | otherwise = do println "x"
                                                                            formatedPrint a (index + 1)

main = do primeiraLinha <- getLine
          vars <- return ((map (read) (words primeiraLinha)) :: [Int])
          dc <- return (newDataCenter (vars!!0) (vars!!1))
          dc <- insereOcupados (vars!!2) dc
          servers <- getContents
          servers <- return sortRatios(leServers 0 (lines servers))
          dc <- return (fillDataCenter dc servers)
          pools <- return (createPools (vars!!3) (vars!!0))
          -- a distribui vai dar um tuplo "resposta" com (pools, servers)
          resposta <- return (distribui pools dc)
          --a lista final de servidores vai ter a localização dos servidores nas pools
          printResposta fst(resposta)
          -- print capacidade_minima_garantida



