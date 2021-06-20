module A1 where

{-
1) Defina uma função recursiva insere :: Int -> [Int] -> [Int] 
que insere um inteiro na posição correta em uma lista de inteiros já ordenada. 
Mostre o passo-a-passo da aplicação de insere 3 [1,2,4,5]
exercício 1 
-}
insere :: Int -> [Int] -> [Int]  
insere x [] = [x]
insere x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insere x ys

{-
insere x [y, ys] ******
insere 3 [1,2,4,5]
1 : insere 3 [2,4,5]
1 : (2 : insere 3 [4,5])
1 : (2 : (3 : 4 : [5]))
1 : (2 : (3 : 4 : 5))
[1,2,3,4,5] 
-}

{-
2) Usando a função insere, defina a função ordenaInsere :: [Int] -> [Int] 
que ordena uma lista de inteiros em ordem crescente usando o algoritmo de ordenação por inserção.
Considere na sua função que uma lista vazia já está em ordem e que para ordenar basta inserir um elemento na posição correta no restante da lista que já deve estar ordenado.
-}
ordenaInsere :: [Int] -> [Int]
ordenaInsere [] = []
ordenaInsere (x:xs) = insere x (ordenaInsere xs) 

{-
ordenaInsere [3,2,1,4]
insere 3 (ordenaInsere [2,1,4])
...
insere 3 (ordenaInsere 2 ( ordenaInsere 1 (ordenaInsere 4 [])))
insere 3 (ordenaInsere 2 ( ordenaInsere 1 ([4])))
insere 3 (ordenaInsere 2 ( [1,4]))
insere 3 ([1,2,4])
[1,2,3,4]
-}

{-
3) Defina uma função recursiva uneOrdenado :: [Int] -> [Int] -> [Int] 
que une duas listas já ordenadas em ordem crescente a uma terceira lista resultante que também deve estar em ordem crescente.
-}
uneOrdenado :: [Int] -> [Int] -> [Int]
uneOrdenado [] x          = x
uneOrdenado x []          = x
uneOrdenado (x:xs) (y:ys) | y<x         = y : uneOrdenado (x:xs) ys
uneOrdenado (x:xs) (y:ys) | otherwise   = x : uneOrdenado xs (y:ys)

{- 
uneOrdenado [1,2,3] [4,5,6]
1 : uneOrdenado [2,3] (4 : [5,6])
1 : (2 : uneOrdenado [3] (4: [5,6]))
1 : (2: (3) (4: [5,6]))
1 : (2 : 3 (4 : [5,6]))
1 : (2 : 3 ([4,5,6]))
1 : (2 : [3,4,5,6])
1 : [2,3,4,5,6]
[1,2,3,4,5,6]
-}

{-
4) Usando a função uneOrdenado, defina uma função ordenaUne :: [Int] -> [Int] 
que particiona sucessivamente uma lista na metade até atingir partições de tamanho 1 para 
então ordenar as partições através da  função uneOrdenado até atingir uma lista ordenada. 
Considere uma lista vazia e a uma lista com um elemento como ordenadas na sua definição.
-}
ordenaUne :: [Int] -> [Int]
ordenaUne []  = []
ordenaUne [x] = [x]
ordenaUne xs = uneOrdenado (ordenaUne ys) (ordenaUne zs)
            where (ys,zs)     = splitAt ((length xs) `div` 2) xs
{-


ß
5) Explique a função padrão zipWith cuja definição é a seguinte:
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f _ _           = []
zipWith f (x:xs) (y:ys) = f x y : zipWith (f xs ys)

A função ZipWith tem como objetivo entrelassar os elementos entre duas listas utilizando as regras de uma outra função passada como parâmetro.
Por conta disso, temos três parametros, o primeiro uma função que recebe dois elementos quaisquer e os substitui por uma lista.




ZipWith recebe uma função e duas listas como parâmetros e então junta as duas listas aplicando a função entre os elementos correspondentes.
O primeiro parâmetro é uma função que recebe duas coisas e produz uma terceira. O segundo e o terceiro parâmetro são listas. O resultado é também uma lista.
O primeiro tem que ser uma lista de as, porque a função de junção recebe as como primeiros argumentos.
A segunda tem que ser uma lista de bs, porque o segundo argumento da função é do tipo b. O resultado é uma lista de cs.
Se a declaração do tipo de uma função diz que ela aceita uma função a -> b -> c como parâmetro, ela também aceitará uma função a -> a -> a, mas não o contrário!
O corpo da função no último pattern é também parecido com zip normal, mas ele não faz (x,y), e sim f x y.
Uma única função de alta ordem pode ser usada para uma enorme variedade de tarefas se é suficientemente geral.
-}

{-
6) A função cresc determina se uma lista está em ordem crescente:

cresc :: (Ord a) => [a] -> Bool
cresc []       = True
cresc [x]      = True
cresc (x:y:xs) = (x <= y) && cresc (y:xs)
-}

cresc :: (Ord a) => [a] -> Bool
cresc xs = and ( zipWith (<=) xs (tail xs))


{-
7) Dê uma definição para a função
disjuntas :: (Ord a) => [a] -> [a] -> Bool
que recebe duas listas em ordem crescente e determina 
se as mesmas não possuem nenhum elemento em comum, isto é, se são disjuntas.
-}
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