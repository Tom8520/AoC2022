import Data.List

handleCommand:: String -> Int -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int), [(Int, Int)])
handleCommand _ 0 h t = (h, t, [])
handleCommand c n (hx, hy) (tx, ty)
  = (nh, nt, nt':r)
  where
    nh'
      | c == "U" = (hx, hy+1)
      | c == "D" = (hx, hy-1)
      | c == "L" = (hx-1, hy)
      | c == "R" = (hx+1, hy)

    nt'
      | c == "U" && ty < hy = (hx, hy)
      | c == "D" && ty > hy = (hx, hy)
      | c == "L" && tx > hx = (hx, hy)
      | c == "R" && tx < hx = (hx, hy)
      | otherwise           = (tx, ty)


    (nh, nt, r) = handleCommand c (n-1) nh' nt'


part1:: (Int, Int) -> (Int, Int) -> IO [(Int, Int)]
part1 h t = do
    input <- getLine

    if input == "END" then do
        return [(0, 0)]
    else do
        let w = words input
            n = read (last w) :: Int

        let (nh, nt, v) = handleCommand (head w) n h t

        rest <- part1 nh nt

        return (v++rest)

handleCommands':: String -> Int -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
handleCommands' _ 0 pos = (pos, [])
handleCommands' c n pos 
  = (np', head np : v)
  where
    newPos [(hx, hy)]
      | c == "U" = [(hx, hy+1)]
      | c == "D" = [(hx, hy-1)]
      | c == "L" = [(hx-1, hy)]
      | c == "R" = [(hx+1, hy)]
    newPos ((tx, ty):ps)
      | dhx == 0 && hy > phy && ty < phy = (phx, phy) : rest
      | dhx == 0 && hy < phy && ty > phy = (phx, phy) : rest
      | dhy == 0 && hx < phx && tx > phx = (phx, phy) : rest
      | dhy == 0 && hx > phx && tx < phx = (phx, phy) : rest
      | dhx /= 0 && dhy /= 0 && adx == 2 && ady == 2 = (phx, phy) : rest
      | dhx /= 0 && dhy /= 0 && adx == 2 = (phx, hy) : rest
      | dhx /= 0 && dhy /= 0 && ady == 2 = (hx, phy) : rest
      | otherwise = (tx, ty) : rest
      where
        rest = newPos ps
        (hx, hy) = head rest
        (phx, phy) = head ps
        (dhx, dhy) = (hx - phx, hy - phy)
        (adx, ady) = (abs (tx - hx), abs (ty - hy))



    np = newPos pos

    (np', v) = handleCommands' c (n-1) np

printGrid:: [(Int, Int)] -> IO ()
printGrid ps = do
    let n = 5
    mapM_ (\x -> do print x) [[if (j, i) `elem` ps then '#' else '.' | j <- [-n..n]]| i <- [-n..n]]
    putStrLn ""


part2:: [(Int, Int)] -> IO [(Int, Int)]
part2 pos = do
    --printGrid ((0, 0):pos)
    input <- getLine

    if input == "END" then do
        return []
    else do
        let w = words input
            n = read (last w) :: Int

        let (np, v) = handleCommands' (head w) n pos

        rest <- part2 np

        return (rest ++ v)

main = do
    ans <- part2 (replicate 10 (0, 0))

    --print ans

    print $ (length . nub) ans
