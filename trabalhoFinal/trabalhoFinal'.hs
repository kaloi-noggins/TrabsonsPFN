import System.IO
import Data.Char(toLower)

main = do 
    textFile <- readFile "oof.txt"
    let lowCase = map toLower textFile
    let sanitized = sanitize (lowCase)
    print sanitized

-- Remove pontuação
sanitize xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'") ]