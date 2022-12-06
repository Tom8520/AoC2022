getTowers:: IO [String]
getTowers = do
    input <- getLine

    let chars = [input!!i | i <- [1,5..length input]]

    if head chars == '1' then do
        return ["" | i <- [1..length chars]]
    else do
        rest <- getTowers
        return [if chars!!i == ' ' then rest!!i else chars!!i : rest!!i| i <- [0..length chars-1]]

applyCommand:: Int -> Int -> Int -> [String] -> [String]
applyCommand 0 _ _ t = t
applyCommand c s e t
  | null (t!!s) = t
  | otherwise   = applyCommand (c-1) s e t'
  where
    char = head (t!!s)
    ns = tail (t!!s)
    ne = char:(t!!e)
    t' = [if i == e then ne else if i == s then ns else  t!!i | i <- [0..length t-1]]

part1:: IO String
part1 = do
    towers <- getTowers
    blank <- getLine

    part1' towers

    where
        part1':: [String] -> IO String
        part1' t = do
            input <- getLine

            if input == "END" then do
                return [head s | s <- t]
            else do
                let ws = words input
                let num = read $ ws!!1 :: Int
                let start = read $ ws!!3 :: Int
                let end = read $ ws!!5 :: Int
                part1' $ applyCommand num (start-1) (end-1) t

applyCommand':: Int -> Int -> Int -> [String] -> [String]
applyCommand' c s e t
   = t'
  where
    m = min c (length (t!!s))
    ss = take m (t!!s)
    ns = drop m (t!!s)
    ne = ss++(t!!e)
    t' = [if i == e then ne else if i == s then ns else  t!!i | i <- [0..length t-1]]

part2:: IO String
part2 = do
    towers <- getTowers
    blank <- getLine

    part2' towers

    where
        part2':: [String] -> IO String
        part2' t = do
            input <- getLine

            if input == "END" then do
                return [head s | s <- t]
            else do
                let ws = words input
                let num = read $ ws!!1 :: Int
                let start = read $ ws!!3 :: Int
                let end = read $ ws!!5 :: Int
                part2' $ applyCommand' num (start-1) (end-1) t


main:: IO ()
main = do
    x <- getTowers
    print x
