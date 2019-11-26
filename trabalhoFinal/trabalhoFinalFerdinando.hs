--ESSE É O MAIN

import Data.List
import Data.Char
-- Tira as pontuações e símbolos

tirar [] = []
tirar (x:xs) = if isPunctuation x || isSymbol x then tirar xs else x: tirar xs


-- Separa cada palavra da linha

separar _ [] = []
separar n (x:xs) = (n,x):separar n xs

allNumWords xs = foldr (\(n,s) -> (++) (separar n (words s))) [] xs

--Função que ordena alfabeticamente palavras
trocar [] = []
trocar ((n,s):xs) = (s,n): trocar xs

sortLs xs = trocar (sort ( trocar xs))

--Agrupamento
enumerar _ [] = []
enumerar a ((n,s):xs)
    | a == s = n:enumerar a xs
    | otherwise = enumerar a xs

almalgamate xs = foldr (\(n,s) -> (:) ((enumerar s xs),s) ) [] xs

--Eliminar repetidos

qtd _ [] = 0
qtd a (x:xs)
    | a == x = 1+(qtd a xs)
    | otherwise = qtd a xs

remover _ [] = []
remover a (x:xs) = if x==a then remover a xs else x:remover a xs

shorten [] = []
shorten xxs@(x:xs)
    |(qtd x xs) == 0 = x:shorten xs
    -- let lowCase = map toLower textFile
    -- let sanitized = sanitize lowCase
    -- let linas
    |otherwise = x:shorten (remover x xs)

------------------------
printLinhas [] = putStrLn "Fim"
printLinhas ((n,s):xs) = do
    putStr ((show n) ++ "-")
    putStrLn s
    printLinhas xs
    
numerar _ [] = []
numerar n (x:xs) = (n,x):numerar (n+1) xs

resultado xs =(foldr (\(n,s) -> (++) (show s ++ " - " ++ show n ++"\n")) "" xs)

main = do
    texto <- (readFile "oof.txt")
    let linhas = lines $ tirar texto
    let result = shorten $ almalgamate $ sortLs $ allNumWords (numerar 1 linhas)
    putStr $ resultado result