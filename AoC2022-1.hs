import Text.Read (readMaybe)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.List

part1:: Int -> IO Int
part1 c = do
    input <- getLine

    let num = readMaybe input :: Maybe Int

    if isJust num
        then do
            part1 ( c + fromJust num)
        else do
            if input == "END" then do
                return c
            else do
                n <- part1 0
                return $ max c n

part2:: IO Int
part2 
  = do
    nums <- part2' 0
    return $ sum $ take 3 $ reverse $ sort nums
  where
    part2':: Int -> IO [Int]
    part2' c = do
        input <- getLine

        let num = readMaybe input :: Maybe Int

        if isJust num
            then do
                part2' ( c + fromJust num)
            else do
                if input == "END" then do
                    return [c]
                else do
                    n <- part2' 0
                    return $ c:n



main:: IO ()
main = do
    n <- part1 0
    print n
