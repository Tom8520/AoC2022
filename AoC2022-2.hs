import Data.Char

getScorePart1:: String -> String -> Int
getScorePart1 a b
  | x == y                   = y + 3
  | mod (x + 1) 3 == mod y 3 = y + 6
  | otherwise                = y
  where
    x = ord (head a) - ord 'A' + 1
    y = if b == "X" then 1 else if b == "Y" then 2 else 3

part1:: Int -> IO Int
part1 c = do
    input <- getLine

    if input == "END" then do
        return c
    else do
        let ws = words input
        part1 $ c + getScorePart1 (head ws) (last ws)

getScorePart2:: String -> String -> Int
getScorePart2 a b
  | b == "X"  = mod (x + 1) 3 + 1
  | b == "Y"  = 3 + x
  | otherwise = 7 + mod x 3
  where
    x = ord (head a) - ord 'A' + 1

part2:: Int -> IO Int
part2 c = do
    input <- getLine

    if input == "END" then do
        return c
    else do
        let ws = words input
        part2 $ c + getScorePart2 (head ws) (last ws)

main:: IO ()
main = do
    ans <- part2 0
    print ans