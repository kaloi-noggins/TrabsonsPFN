--ex1
reversoAninhado x = map reverse x

--ex2
aFrente n x = map (n:) x

--ex3
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

semListaVazia x = inverte $ foldl (\acc x -> if length x > 0 then x:acc else acc) [] x
--ex4
cNroPar [] = []
cNroPar (x:xs) = semListaVazia $ checaPar x : cNroPar xs
    where checaPar x = if length (filter even x) > 0 then x else [] 
--ex5

--ex6

--ex7

--ex8

--ex9

--ex10

--ex11

--ex12
