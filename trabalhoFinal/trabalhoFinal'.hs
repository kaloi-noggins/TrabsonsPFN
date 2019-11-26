-- import System.IO
import Data.Char(toLower)

main = do 
    textFile <- readFile "oof.txt"
    let lowCase = map toLower textFile -- Converte o texto para lowercase (auxilio para analise)
    let sanitized = sanitize lowCase -- Remove pontuacao (auxilio para analise)
    let linhas = lines sanitized -- a) Separar o documento em linhas
    let linhasNumeradas = numLines 1 linhas -- b) Numerar as linhas do documento
    let -- c) Associar a cada ocorrência de uma palavra do documento, o número da linha em que essa palavra ocorre

    print linhasNumeradas

-- Removedor de pontuação
sanitize xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'") ]

-- Numerador de linhas
numLines _ [] = []
numLines n (x:xs) = (n,x) : numLines (n+1) xs
