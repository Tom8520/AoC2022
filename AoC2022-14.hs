import Data.List
import Data.Maybe

getInput :: IO [(Int, Int)]
getInput = do
    line <- getLine

    if line == "END" then do
        return []
    else do
        let w = words line

        let c = [read ("(" ++ w!!x ++ ")") :: (Int, Int) | x <- [0,2..length w]]

        let getRange i f 
              = [a..b]
              where
                a = min (f (c!!i)) (f (c!!(i+1)))
                b = max (f (c!!i)) (f (c!!(i+1)))


        let r = [(x, y) | i <- [0..length c-2], x <- getRange i fst, y <- getRange i snd]

        rest <- getInput

        return $ nub (rest ++ r)

simSand:: (Int, Int) -> [(Int, Int)] -> Maybe (Int, Int)
simSand (x, y) ps
  | y > 500                 = Nothing
  | (x, y+1) `notElem` ps   = simSand (x, y+1) ps
  | (x-1, y+1) `notElem` ps = simSand (x-1, y+1) ps
  | (x+1, y+1) `notElem` ps = simSand (x+1, y+1) ps
  | otherwise               = Just (x, y)

getSand:: [(Int, Int)] -> Int
getSand ps
  | isNothing s = 0
  | otherwise   = 1 + getSand (fromJust s : ps)
  where
    s = simSand (500, 0) ps

part1:: IO Int
part1 = do
    inputs <- getInput

    return $ getSand inputs

simSand':: (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
simSand' (x, y) ps my
  | y > my                  = [(x, y)]
  | (x, y+1) `notElem` ps   = (x, y) : simSand' (x, y+1) ps my
  | (x-1, y+1) `notElem` ps = (x, y) : simSand' (x-1, y+1) ps my
  | (x+1, y+1) `notElem` ps = (x, y) : simSand' (x+1, y+1) ps my
  | otherwise               = [(x, y)]

getSand':: [(Int, Int)] -> [(Int, Int)] -> Int -> Int
getSand' ps (x:xs) my
  | s == (500, 0) = 1
  | otherwise     = 1 + getSand' (s : ps) (ss ++ xs) my
  where
    (s:ss) = reverse $ simSand' x ps my

part2:: IO Int
part2 = do
    inputs <- getInput

    let my = maximum [y | (x, y) <- inputs]

    return $ getSand' inputs [(500, 0)] my
