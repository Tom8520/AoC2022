split:: String -> String -> [String]
split s seps
  = split' s ""
  where
    split' [] c = [c]
    split' (s:ss) c
      | s `elem` seps = c:split' ss ""
      | otherwise     = split' ss (c++[s])

splitInput:: String -> [Int]
splitInput s 
  = map (\x -> read x :: Int) (split s "-,")

contains:: Int -> Int -> Int -> Int -> Int
contains a b c d
  | a >= c && b <= d = 1
  | c >= a && d <= b = 1
  | otherwise        = 0

part1:: IO Int
part1 = do
    input <- getLine

    if input == "END" then do
        return 0
    else do
        let [a, b, c, d] = splitInput input
        let cont = contains a b c d

        rest <- part1

        return (rest + cont)

overlap:: Int -> Int -> Int -> Int -> Int
overlap a b c d
  | c >= a && c <= b = 1
  | a >= c && a <= d = 1
  | otherwise        = 0

part2:: IO Int
part2 = do
    input <- getLine

    if input == "END" then do
        return 0
    else do
        let [a, b, c, d] = splitInput input
        let over = overlap a b c d

        rest <- part2

        return (rest + over)

