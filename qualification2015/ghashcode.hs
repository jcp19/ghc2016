module Main where

data Server = Vazio | Ocupado | Serv Int Int Int deriving (Show, Read, Eq)
-- id capacity size 
type DataCenter = [[Server]]

newDataCenter :: Int -> Int -> DataCenter
newDataCenter x y = replicate x (replicate y Vazio)

-- insere na posição escolhida do DC, nao verifica se é possivel inserir na posiçao escolhida, isso deve ser verificado antes
insertPosDataCenter :: DataCenter -> Int -> Int -> Server -> DataCenter
insertPosDataCenter dc x y a@(Serv id cap size) = (take x dc) ++ [take y (dc!!x) ++ [a] ++ (replicate (size-1) Ocupado) ++ drop (y+size) (dc!!x)] ++ (drop (x+1) dc)
insertPosDataCenter dc x y a = (take x dc) ++ [take y (dc!!x) ++ [a] ++ drop (y+1) (dc!!x)] ++ (drop (x+1) dc)

insereOcupados :: Int -> DataCenter -> IO DataCenter
-- quantos_ocupados -> lista
insereOcupados 0 dc = return dc
insereOcupados x dc = do pos <- getLine
                         vars <- return ((map (read) (words pos)) :: [Int])
                         newDc <- return (insertPosDataCenter dc (vars!!0) (vars!!1) Ocupado)
                         newDc <- insereOcupados (x-1) newDc
                         return newDc



main = do primeiraLinha <- getLine
          vars <- return ((map (read) (words primeiraLinha)) :: [Int])
          dc <- return (newDataCenter (vars!!0) (vars!!1))
          dc <- insereOcupados (vars!!2) dc
          servers <- getContents
          print servers


