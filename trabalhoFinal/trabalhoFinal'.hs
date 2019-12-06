import System.IO
import Data.Char(toLower)

main = do 
    textFile <- readFile "oof.txt"
    let lowCase = map toLower textFile -- Converte o texto para lowercase (auxilio para analise)
    let sanitized = sanitize lowCase -- Remove pontuacao (auxilio para analise)
    let linhas = lines sanitized -- a) Separar o documento em linhas
    let linhasNumeradas = numLines 1 linhas -- b) Numerar as linhas do documento
    let listaPalvrasIndices = numPalavras (linhasNumeradas)
   -- let listaAlfabetica = sortStr linhasNumeradas
    print listaPalvrasIndices

-- Removedor de pontuação
sanitize xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'") ]

-- Numerador de linhas
numLines _ [] = []
numLines n (x:xs) = (n,x) : numLines (n+1) xs


-- Numerador de palavras
separar _ [] = []
separar n (x:xs) = (n,x) : separar n xs

numPalavras xs = foldr (\(n,s) -> (++) $ separar n (words s)) [] xs