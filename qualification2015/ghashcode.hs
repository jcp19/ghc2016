import Data.List

module Main where

data Server = Vazio | Ocupado | Serv Int Int Int Int Int Int deriving (Show, Read, Eq)
-- id capacity size x y pool
type DataCenter = [[Server]]

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

linha_menorCap_ondeCabe :: DataCenter -> Server -> Int
-- falta ordenar por capacidade
linha_menorCap_ondeCabe dc (Serv _ _ size _ _ _) =  head (sort (filter (\(x,y) -> maxSlotFree x >= size) (indexer dc)))

maxSlotFree :: [Servers] -> Int
maxSlotFree [] = 0
maxSlotFree k = max( (length (takeWhile (\x -> x == Vazio)) k), (maxSlotFree (DropWhile (\x -> x /= Vazio) (DropWhile (\x -> x == Vazio) ) k)) )

fillDataCenter :: DataCenter -> [Servers] -> DataCenter
-- dado o data center, enche-o com os servidores (a lista de Servers está ordenada decrescentemente por racio)
fillDataCenter dc [] = dc
fillDataCenter dc (h:t) | linha_menorCap_ondeCabe dc h == -1 = fillDataCenter dc t
                        | otherwise = insertPosDataCenter dc (linha_menorCap_ondeCabe dc h) (melhor_pos dc (linha_menorCap_ondeCabe dc h) h) h


main = do primeiraLinha <- getLine
          vars <- return ((map (read) (words primeiraLinha)) :: [Int])
          dc <- return (newDataCenter (vars!!0) (vars!!1))
          dc <- insereOcupados (vars!!2) dc
          servers <- getContents
          servers <- return (leServers 0 (lines servers))
          print servers



