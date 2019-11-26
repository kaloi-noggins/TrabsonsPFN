import System.IO
import Data.Char(toLower)

main = do 
    textFile <- readFile "oof.txt"
    let lowCase = map toLower textFile
    let sanitized = sanitize lowCase
    let linhas = lines sanitized
    let linhasNumeradas = numLinhas 0 linhas
    print linhasNumeradas

-- Removedor pontuação
sanitize xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'") ]

--Numerador de linhas
numLinhas _ [] = []
numLinhas n (x:xs) = (n,x) : numLinhas (n+1) xs