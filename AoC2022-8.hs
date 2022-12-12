import Data.Char
checkTree:: [[Int]] -> Int -> Int -> Bool
checkTree g i j
  = r || c
  where
    r = checkRow (g!!i) j
    col = [x!!j | x <- g]
    c = checkRow col i

checkRow:: [Int] -> Int -> Bool
checkRow l i
  = x > a || x > b
  where
    x = l!!i
    a = maximum $ (-1):take i l
    b = maximum $ (-1):drop (i+1) l

getInput:: IO [[Int]]
getInput = do
    line <- getLine

    if line == "END" then do
        return []
    else do
        let row = [ord x - ord '0' | x <- line]
        rest <- getInput
        return (row:rest)

part1:: IO Int
part1 = do
    g <- getInput

    let n = length g
        m = length (head g)
        ls = [checkTree g i j | i <- [0..n-1], j <- [0..m-1]]

    let visible = length [x | x <- ls, x]

    return visible

checkDist:: [[Int]] -> Int -> Int -> Int
checkDist g i j
  = r * c
  where
    r = checkRowDist (g!!i) j
    col = [x!!j | x <- g]
    c = checkRowDist col i

checkRowDist:: [Int] -> Int -> Int
checkRowDist l i
  = a'' * b''
  where
    x = l!!i
    a = reverse $ take i l
    b = drop (i+1) l

    a' = length (takeWhile (< x) a)
    b' = length (takeWhile (< x) b)

    a'' = a' + min 1 (length a - a')
    b'' = b' + min 1 (length b - b')

part2:: IO Int
part2 = do
    g <- getInput

    let n = length g
        m = length (head g)
        ls = [checkDist g i j | i <- [0..n-1], j <- [0..m-1]]

    let d = maximum ls

    return d  
