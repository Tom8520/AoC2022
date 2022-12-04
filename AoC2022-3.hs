import Data.Char
import Data.List

getNum:: Char -> Int
getNum c
  | c `elem` ['a'..'z'] = ord c - ord 'a' + 1
  | otherwise           = ord c - ord 'A' + 27

findSame:: String -> String -> Int
findSame a b 
  = findSame' (sort (map getNum a)) (sort (map getNum b))
  where
    findSame':: [Int] -> [Int] -> Int
    findSame' (a:as) (b:bs)
      | a == b    = a
      | a < b     = findSame' as (b:bs)
      | otherwise = findSame' (a:as) bs

part1:: IO Int
part1 = do
    input <- getLine

    if input == "END!" then do
        return 0
    else do
        let mid = length input `div` 2
        let f   = take mid input
        let s   = drop mid input

        rest <- part1
        return $ findSame f s + rest

findSame3:: String -> String -> String -> Int
findSame3 a b c
  = findSame3' (sort (map getNum a)) (sort (map getNum b)) (sort (map getNum c))
  where
    findSame3':: [Int] -> [Int] -> [Int] -> Int
    findSame3' (a:as) (b:bs) (c:cs)
      | a == b && b == c       = a
      | minimum [a, b, c] == a = findSame3' as (b:bs) (c:cs)
      | minimum [a, b, c] == b = findSame3' (a:as) bs (c:cs)
      | otherwise              = findSame3' (a:as) (b:bs) cs

part2:: IO Int
part2 = do
    input <- getLine

    if input == "END!" then do
        return 0
    else do
        line2 <- getLine
        line3 <- getLine

        let num = findSame3 input line2 line3

        rest <- part2
        return $ num + rest