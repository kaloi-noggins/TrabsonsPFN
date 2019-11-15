listaA = [1..5]
listaB = [2..6]
listaT = [1,2,3,2]
listaX = ['a','b','c','d','e','f']
listaY = [0,3,2,3]

-- 1
-- recebe um número inteiro positivo e retorna o n-ésimo elemento da seqüência de Fibonacci.
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- 2
-- recebe uma lista qualquer e um número inteiro positivo para retornar o nésimo elemento da lista.
elemento (x:xs) 1 = x
elemento (x:xs) n = elemento xs (n-1)

-- 3
-- recebe um elemento e uma lista e verifica se o elemento pertence à lista.
pertence n [] = False
pertence n (x:xs) | n == x = True
                  | otherwise = pertence n xs

-- 4
-- recebe um elemento e uma lista e retorna o número de ocorrências do elemento na lista.
contaOcorrencias n (x:xs) = contaOcorrencias' n (x:xs) 0

contaOcorrencias' n [] a = a
contaOcorrencias' n (x:xs) a | n == x = contaOcorrencias' n xs (a+1)
                             | otherwise = contaOcorrencias' n xs a

-- 5
-- recebe um elemento e uma lista e verifica se existe uma única ocorrência do elemento na lista.
unicaOcorrencia n [] = False
unicaOcorrencia n (x:xs) | n == x = unicaOcorrencia' n xs
                         | otherwise = unicaOcorrencia n xs

unicaOcorrencia' n [] = True
unicaOcorrencia' n (x:xs) | n == x = False
                          | otherwise = unicaOcorrencia' n xs

-- 6
-- recebe um número inteiro e uma lista de inteiros e retorna uma lista com os números que são maiores que o fornecido.
maioresQue n (x:xs) | n < x = x : maioresQue n xs
                    | otherwise = maioresQue n xs

-- 7
-- recebe um elemento e uma lista e retorna a lista sem a primeira ocorrência do elemento.
remover n [] = []
remover n (x:xs) | n == x = remover' xs
                 | otherwise = x : remover n xs
remover' [] = []
remover' (x:xs) = x : remover' xs

-- 8
-- recebe uma lista qualquer e retorna outra lista sem repetição de elementos.
removerRepetidos _ [] = []
removerRepetidos x ()

-- 9
-- recebe um numero natural n e uma lista de inteiros e retorna uma lista com os n maiores números sem alterar a ordem entre os elementos.
-- maiores

-- 10
-- recebe um número inteiro n positivo e retorna a lista [1, -1, 2, -2, 3, -3, ... n, -n]
geraSequencia n = geraSequencia' n (n-1)

geraSequencia' n (-1) = []
geraSequencia' n m = n - m : -(n - m) : geraSequencia' n (m-1)

-- 11
-- recebe duas listas e retorna outra lista com os elementos das listas originais intercalados.
intercala [] [] = []
intercala [] (y:ys) = y : intercala [] ys
intercala (x:xs) [] = x : intercala xs []
intercala (x:xs) (y:ys) = x : y : intercala xs ys

-- 12
-- recebe duas listas quaisquer que não contenham elementos repetidos e retorna uma nova com todos os elementos das duas listas originais (sem repetição).
-- uniao

-- 13
-- recebe dois numeros naturais n e m, e retorna uma lista com n elementos, onde o primeiro é m, o segundo é m+1, etc...
sequencia 0 m = []
sequencia n m = m : sequencia (n-1) (m+1) 

-- 14
-- recebe um número natural, uma lista e retorna uma nova lista onde a posição dos elementos mudou como se eles tivessem sido "rodados".
-- rodarEsquerda 

-- 15
-- recebe uma lista qualquer e uma lista de posições, retorna uma lista com os elementos da primeira que estavam nas posições indicadas.
-- seleciona

-- 16
-- recebe uma string e verifica se ela é uma palíndrome ou nao.
palindrome [] = True
palindrome (x:xs) | xs == [] = True
                  | x /= last xs = False
                  | otherwise = palindrome (init xs)

-- 17
-- recebe um número natural e retorna a soma de seus dígitos.
somaDigitos n | n == 0 = 0
              | otherwise = (mod n 10) + somaDigitos ((n - (n `mod` 10)) `div` 10)

-- 18
-- um programa que dada uma lista, retorne uma tupla listalista (de inteiros) onde a lista da esquerda contém os números impares e a lista da direita os números pares.
separaParImpar (x:xs) = (separaPar (x:xs), separImpar (x:xs))

separaPar [] = []
separaPar (x:xs) | x `mod` 2 == 0 = x: separaPar xs
                 | otherwise = separaPar xs

separImpar [] = []
separImpar (x:xs) | x `mod` 2 /= 0 = x:separImpar xs
                  | otherwise = separImpar xs