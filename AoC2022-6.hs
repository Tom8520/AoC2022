import Data.List
getFirstMarker:: String -> Int
getFirstMarker s
  | l == 4    = 4
  | otherwise = 1 + getFirstMarker (tail s)
  where
    s' = take 4 s
    l = length (nub s')

part1:: IO Int
part1 = do
    input <- getLine
    let m = getFirstMarker input
    return m

getFirstMessageMarker:: String -> Int
getFirstMessageMarker s
  | l == 14   = 14
  | otherwise = 1 + getFirstMessageMarker (tail s)
  where
    s' = take 14 s
    l = length (nub s')

part2:: IO Int
part2 = do
    input <- getLine
    let m = getFirstMessageMarker input
    return m
    