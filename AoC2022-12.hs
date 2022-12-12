import Data.List
import Data.Char

getInput:: IO [String]
getInput = do
    line <- getLine

    if line == "END" then do
        return []
    else do
        rest <- getInput

        return (line:rest)

getPoint:: [String] -> Char -> (Int, Int)
getPoint g c
  = getPoint' g 0
  where
    getPoint' g n
      | null i    = getPoint' g (n+1)
      | otherwise = (n, head i)
      where
        r = g!!n
        i = [i | i <- [0..length r-1], r!!i == c]

getNxt:: [[Int]] -> (Int, Int) -> [(Int, Int)]
getNxt g (r, c)
  = [x | x <- [l', r', u', d'], x /= (-1, -1)]
  where
    x = (g!!r)!!c + 1
    n = length g
    m = length (g!!0)

    l' = if c > 0 && (g!!r)!!(c-1) <= x then (r, c-1) else (-1, -1)
    r' = if c < m-1 && (g!!r)!!(c+1) <= x then (r, c+1) else (-1, -1)
    u' = if r > 0 && (g!!(r-1))!!c <= x then (r-1, c) else (-1, -1)
    d' = if r < n-1 && (g!!(r+1))!!c <= x then (r+1, c) else (-1, -1)

bfs:: [[Int]] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)] -> Int -> Int
bfs g q e v n
  | e `elem` v = n
  | otherwise  = bfs g nxt e nv (n+1)
  where
    nxt = [x | x <- (nub . concatMap (getNxt g)) q, x `notElem` v]

    nv = v ++ nxt

part1:: IO Int
part1 = do
    g <- getInput

    let g' = [map (\x -> if x == 'S' then 0 else if x == 'E' then 25 else ord x - ord 'a') r | r <- g]

    let start = getPoint g 'S'
        end   = getPoint g 'E'

    return $ bfs g' [start] end [] 0

getNxt':: [[Int]] -> (Int, Int) -> [(Int, Int)]
getNxt' g (r, c)
  = [x | x <- [l', r', u', d'], x /= (-1, -1)]
  where
    x = (g!!r)!!c - 1
    n = length g
    m = length (g!!0)

    l' = if c > 0 && (g!!r)!!(c-1) >= x then (r, c-1) else (-1, -1)
    r' = if c < m-1 && (g!!r)!!(c+1) >= x then (r, c+1) else (-1, -1)
    u' = if r > 0 && (g!!(r-1))!!c >= x then (r-1, c) else (-1, -1)
    d' = if r < n-1 && (g!!(r+1))!!c >= x then (r+1, c) else (-1, -1)

bfs':: [[Int]] -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int
bfs' g q v n
  | 0 `elem` map (\(r, c) -> (g!!r)!!c) v = n
  | otherwise                             = bfs' g nxt nv (n+1)
  where
    nxt = [x | x <- (nub . concatMap (getNxt' g)) q, x `notElem` v]

    nv = v ++ nxt

part2:: IO Int
part2 = do
    g <- getInput

    let g' = [map (\x -> if x == 'S' then 0 else if x == 'E' then 25 else ord x - ord 'a') r | r <- g]

    let start = getPoint g 'S'
        end   = getPoint g 'E'

    return $ bfs' g' [end] [] 0