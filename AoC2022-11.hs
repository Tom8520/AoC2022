import Prelude hiding (round)
import Data.List

type Monkey = (Int, [Int], Int -> Int, Int, Int, Int, Int)

printM:: Monkey -> IO ()
printM (i, s, _, d, t, f, _)= do
    putStr "Monkey "
    print i

    putStr "  Items: "
    putStrLn ("" ++ concat [show x ++ ", " | x <- s])

    putStr "  Div by "
    print d

    putStr "  If true then "
    print t

    putStr "  If false then "
    print f
    putStrLn ""

getInput :: IO [Monkey]
getInput = do
    line <- getLine

    if line == "END" then do
        return []
    else do
        let i = read (init $ last $ words line) :: Int

        line2 <- getLine

        let starting = [read (takeWhile (/=',') x) :: Int | x <- (tail.tail) $ words line2]

        line3 <- getLine

        let w = words line3
        let op
              | last w == "old"        = (\x -> x*x) 
              | (last . init) w == "*" = (\x -> x * (read (last w) :: Int))
              | otherwise              = (\x -> x + (read (last w) :: Int))

        line4 <- getLine

        let d = read ((last . words) line4) :: Int

        line5 <- getLine

        let t = read ((last . words) line5) :: Int

        line6 <- getLine

        let f = read ((last . words) line6) :: Int

        _ <- getLine

        rest <- getInput

        return ((i, starting, op, d, t, f, 0) : rest)

round:: [Monkey] -> Int -> [Monkey]
round ms i
  | i == length ms = ms
  | null is        = round ms (i+1)
  | otherwise      = round ms'' i
  where
    (_, is, op, d, t, f, cnt) = ms!!i

    w = div (op $ head is) 3

    j = if mod w d == 0 then t else f

    ms' = [if k == j then (k, is' ++ [w], op', d', t', f', cnt') else m | m@(k, is', op', d', t', f', cnt') <- ms]
    ms'' = [if k == i then (k, tail is', op', d', t', f', cnt'+1) else m | m@(k, is', op', d', t', f', cnt') <- ms']

part1 :: IO Int
part1 = do
    ms <- getInput

    let final = foldl (\a _ -> round a 0) ms (replicate 10000 0)

    let scores = sort [cnt | (_, _, _, _, _, _, cnt) <- final]

    let mb = last scores * (last . init) scores

    return mb

round':: [Monkey] -> Int -> Int -> [Monkey]
round' ms i dt
  | i == length ms = ms
  | null is        = round' ms (i+1) dt
  | otherwise      = round' ms'' i dt
  where
    (_, is, op, d, t, f, cnt) = ms!!i

    -- w = div (op $ head is) 3
    w = mod (op $ head is) dt

    j = if mod w d == 0 then t else f

    ms' = [if k == j then (k, is' ++ [w], op', d', t', f', cnt') else m | m@(k, is', op', d', t', f', cnt') <- ms]
    ms'' = [if k == i then (k, tail is', op', d', t', f', cnt'+1) else m | m@(k, is', op', d', t', f', cnt') <- ms']

part2 :: IO Int
part2 = do
    ms <- getInput

    let d = product [d | (_, _, _, d, _, _, _) <- ms]

    let final = foldl (\a _ -> round' a 0 d) ms (replicate 10000 0)

    let scores = sort [cnt | (_, _, _, _, _, _, cnt) <- final]

    let mb = last scores * (last . init) scores

    return mb
