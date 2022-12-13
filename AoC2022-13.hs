import Data.Char

comp:: String -> String -> Bool
comp "" _ = True
comp _ "" = False
comp ax bx
  | a == "]" && b == "]"              = comp as bs
  | a == "]"                          = True
  | b == "]"                          = False
  | a == "[" && b == "["              = comp as bs
  | a == "["                          = comp as (b++']':bs)
  | b == "["                          = comp (a++']':as) bs
  | (read a :: Int) < (read b :: Int) = True
  | (read b :: Int) < (read a :: Int) = False
  | otherwise                         = comp as bs
  where
    a  = if head ax `elem` ['0'..'9'] then takeWhile (`elem`['0'..'9']) ax else take 1 ax
    as' = drop (length a) ax
    as = if head as' == ',' then tail as' else as'
    
    b  = if head bx `elem` ['0'..'9'] then takeWhile (`elem`['0'..'9']) bx else take 1 bx
    bs' = drop (length b) bx
    bs = if head bs' == ',' then tail bs' else bs'

qsort :: [String] -> [String]
qsort []  = []
qsort [x] = [x]
qsort xs
  = l ++ ps ++ r
  where
    p = head xs
    ps = [x | x <- xs, x == p]
    l = qsort [x | x <- xs, x /= p, comp x p]
    r = qsort [x | x <- xs, x /= p, comp p x]

part1:: Int -> IO Int
part1 n = do
    a <- getLine
    b <- getLine

    sep <- getLine
    
    rest <- if sep == "END" then return 0 else part1 (n+1)

    if comp a b then
        return (n + rest)
    else 
        return rest

getInput:: IO [String]
getInput = do
    a <- getLine
    b <- getLine

    sep <- getLine
    
    rest <- if sep == "END" then return [] else getInput

    return (a:b:rest)

part2:: IO Int
part2 = do
    input <- getInput

    let sorted = qsort ("[[2]]":"[[6]]":input)

    let decode = [x+1 | x <- [0..length sorted-1], sorted!!x == "[[2]]" || sorted!!x == "[[6]]"]

    return $ product decode
