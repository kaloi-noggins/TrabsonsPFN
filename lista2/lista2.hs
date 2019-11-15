{-Alunos: Danilo Sousa Ruiz dos Passos e Kalyl Henings-}

--ex1
reversoAninhado xs = map reverse xs

--ex2
aFrente n xs = map (n:) xs

--ex3
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

semListaVazia x = inverte $ foldl (\acc x -> if length x > 0 then x:acc else acc) [] x
--ex4
cNroPar [] = []
cNroPar (x:xs) = semListaVazia $ checaPar x : cNroPar xs
    where checaPar x = if length (filter even x) > 0 then x else [] 

    --ex5
addNmb _ [] = []
addNmb n (x:xs) = n++x : addNmb n xs

aFrentePar n xs = addNmb n listaListaPares
    where listaListaPares = cNroPar xs

--ex6

--ex7

--ex8

--ex9

--ex10

--ex11

--ex12
