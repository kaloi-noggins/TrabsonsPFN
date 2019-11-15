-- 1. Fibonacci recebe um número inteiro positivo e retorna o enésimo elemento da sequência de Fibonacci.
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n-1)) + (fibonacci (n-2))

-- 2. elemento: recebe uma lista qualquer e um número inteiro positivo para retornar o enésimo elemento da lista.
elementox [] _ = -1
elementox (x:_) 1 = x
elementox (_:xs) n = elementox xs (n-1)

-- 3. pertence: recebe um elemento e uma lista e verifica se o elemento pertence à lista.
pertence _ [] = False
pertence n (x:xs)
    | n == x = True
    | otherwise = pertence n xs

-- 4. contaOcorrencias: recebe um elemento e uma lista e retorna o número de ocorrências do elemento na lista.
contaOco _ [] = 0
contaOco n (x:xs)
    | n == x = 1 + contaOco n xs
    | otherwise = contaOco n xs

-- 5. unicaOcorrencia: recebe um elemento e uma lista e verifica se existe uma única ocorrência do elemento na lista.
unicaOco n (x:xs)
    | contaOco n (x:xs) == 1 = True
    | otherwise = False

-- 6. maioresQue: recebe um número inteiro e uma lista de inteiros e retorna uma lista com os números que são maiores que o fornecido.
maioresQue _ [] = []
maioresQue n (x:xs)
    | n > x = maioresQue n xs
    | otherwise = x : maioresQue n xs

-- 7. remover: recebe um elemento e uma lista e retorna a lista sem a primeira ocorrência do elemento.
remover n [] = []
remover n (x:xs)
    | n /= x = x : remover n xs
    | otherwise = xs

-- 8. removerRepetidos: recebe uma lista qualquer e retorna outra lista sem repetição de elementos.
removerTODOS _ [] = []
removerTODOS x (y:ys)
    | x == y = removerTODOS x ys
    | otherwise = y : removerTODOS x ys

removerRepetidos [] = []
removerRepetidos (x:xs) = x : removerRepetidos (removerTODOS x xs)

-- 9. maiores: recebe um número natural n e uma lista de inteiros e retorna uma lista com os n maiores números sem alterar a ordem entre os elementos.
maiores _ [] = []
maiores n (x:xs)
    | n > length (maioresQue x xs) = x : maiores (n-1) xs
    | otherwise = maiores n xs

-- 10. geraSequencia: recebe um número inteiro n positivo e retorna a lista [1, -1, 2, -2, ... n, -n]
geraSequencia 0 = []
geraSequencia n = geraSequencia (n-1)++[n,-n]

-- 11. intercala: recebe duas listas e retorna outra lista com os elementos das listas originais intercalados.
intercala [] y = y
intercala x [] = x
intercala (x:xs) (y:ys) = x:y:intercala xs ys

-- 12. uniao: recebe duas listas quaisquer que não contenham elementos repetidos e retorna uma nova com todos os elementos das duas listas originais (sem repetição).
iguais _ [] = []
iguais n (x:xs)
    | n==x = iguais n xs
    | n/=x = x:iguais n xs

uniao [] x = x
uniao x [] = x
uniao (x:xs) ys = x:uniao xs (iguais x ys)

-- 13. sequencia: recebe dois números naturais n e m, e retorna uma lista com n elementos, onde o primeiro é m, o segundo é m+1, etc...
sequencia 0 _ = []
sequencia x y = y:sequencia (x-1) (y+1)

-- 14. rodarEsquerda: recebe um número natural, uma lista e retorna uma nova lista onde a posição dos elementos mudou como se eles tivessem sido.
mover n xs
    | (length xs) > n = [indice xs n] ++ mover (n+1) xs
    | (length xs) <= n = xs

rodarEsquerda n xs
    | n <= (length xs) = take (length xs) (mover n xs)
    | n > (length xs) = rodarEsquerda (n-(length xs)) xs

-- 15. seleciona: recebe uma lista qualquer e uma lista de posições, retorna uma lista com os elementos da primeira que estavam nas posições indicadas.
seleciona _ [] = []
seleciona [] _ = []
seleciona ys (x:xs) = [(indice ys x)] : seleciona ys xs

-- 16. palindrome: recebe uma string e verifica se ela é uma palíndromo ou não.
revertelista [] = []
revertelista xs = last xs : revertelista (init xs)

palindrome x
    | (revertelista x) == x = True
    | otherwise = False

-- 17. somaDigitos: recebe um número natural e retorna a soma de seus dígitos.
somaDigitos 0 = 0
somaDigitos x = (somaDigitos (div x 10)) + (mod x 10)

-- 18. separaParImpar: um programa que dada uma lista, retorne uma tupla lista (de inteiros) onde a lista da esquerda contém os números impares e a lista da direita os números pares.
separarParImpar (xs) = ((impar 2 xs),(par 2 xs))

par _ [] = []
par n (x:xs)
    | mod x n == 0 = x : par n xs
    | otherwise = par n xs

impar _ [] = []
impar n (x:xs)
    | mod x n /= 0 = x : impar n xs
    | otherwise = impar n xs

--prelude do marco
indice [] _ = error "deu erro mano"
indice (x:xs) n 
    | n == 0 = x
    |otherwise = indice xs (n-1)