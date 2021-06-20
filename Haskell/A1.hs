module A1 where

-- exercício 1 -------------------
insere :: Int -> [Int] -> [Int]  
insere x [] = [x]
insere x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insere x ys

-- insere x [y, ys] ******
-- insere 3 [1,2,4,5]
-- 1 : insere 3 [2,4,5]
-- 1 : (2 : insere 3 [4,5])
-- 1 : (2 : (3 : 4 : [5]))
-- 1 : (2 : (3 : 4 : 5))
-- [1,2,3,4,5]



-- exercicio 2 -------------------
ordenaInsere :: [Int] -> [Int]
ordenaInsere [] = []
ordenaInsere (x:xs) = insere x (ordenaInsere xs) 

-- ordenaInsere [3,2,1,4]
-- insere 3 (ordenaInsere [2,1,4])
-- ...
-- insere 3 (ordenaInsere 2 ( ordenaInsere 1 (ordenaInsere 4 [])))
-- insere 3 (ordenaInsere 2 ( ordenaInsere 1 ([4])))
-- insere 3 (ordenaInsere 2 ( [1,4]))
-- insere 3 ([1,2,4])
-- [1,2,3,4]



-- exercicio 3 -------------------
uneOrdenado :: [Int] -> [Int] -> [Int]
uneOrdenado [] x          = x
uneOrdenado x []          = x
uneOrdenado (x:xs) (y:ys) | y<x         = y : uneOrdenado (x:xs) ys
uneOrdenado (x:xs) (y:ys) | otherwise   = x : uneOrdenado xs (y:ys)

-- uneOrdenado [1,2,3] [4,5,6]
-- 1 : uneOrdenado [2,3] (4 : [5,6])
-- 1 : (2 : uneOrdenado [3] (4: [5,6]))
-- 1 : (2: (3) (4: [5,6]))
-- 1 : (2 : 3 (4 : [5,6]))
-- 1 : (2 : 3 ([4,5,6]))
-- 1 : (2 : [3,4,5,6])
-- 1 : [2,3,4,5,6]
-- [1,2,3,4,5,6]



-- exercicio 4 -------------------
-- tá errado tem que arrumar
ordenaUne :: [Int] -> [Int]
ordenaUne []  = []
ordenaUne [x] = [x]
ordenaUne xs = uneOrdenado (ordenaUne ys) (ordenaUne zs)
            where (ys,zs)     = splitAt ((length xs)`div` 2) xs



-- exercicio 5 -------------------
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f _ _           = []
-- zipWith f (x:xs) (y:ys) = f x y : zipWith (f xs ys)



-- exercicio 6 -------------------
cresc :: (Ord a) => [a] -> Bool
cresc []       = True
cresc [x]      = True
cresc (x:y:xs) = (x <= y) && cresc (y:xs)


-- exercicio 7 -------------------
disjuntas :: (Ord a) => [a] -> [a] -> Bool
disjuntas [] _ = True
disjuntas (x:xs) ys
  | elem x ys = False
  | otherwise = disjuntas xs ys

{-
[1,2,4] [4,5,6]
elem 1 [4,5,6] --  elem (x) 1 não está contido em (ys) [4,5,6]. Logo, irá para o otherwise iniciar a próxima chamada.
disjuntas [2,4] [4,5,6]
elem 2 [4,5,6] --- elem (x) 2 não está contido em (ys) [4,5,6] entra no otherwise 
disjuntas [4] [4,5,6]
elem 4 [4,5,6] --- elem (x) 4 está contido em (ys) [4,5,6] substitui todas as chamadas por False 
-}

-- que recebe duas listas em ordem crescente e determina
-- se as mesmas não possuem nenhum elemento em comum, isto é, se são disjuntas.