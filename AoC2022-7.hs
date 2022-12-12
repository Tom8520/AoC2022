import GHC.IO.Handle
import System.IO
data Node = Dir String [Node] | File String Int deriving (Eq, Show)

ls:: IO [Node]
ls = do
    nxt <- hLookAhead stdin

    if nxt == '$' || nxt == '#' then do
        return []
    else do
        line <- getLine
        let parts = words line

        rest <- ls
        if head parts == "dir" then do
            return rest
        else do
            let size = read (head parts) :: Int
            return $ File (parts!!1) size : rest


genTree::Node -> IO Node
genTree r@(Dir n l) = do
    nxt <- hLookAhead stdin
    if nxt == '#' then do
        return r
    else do 
        cmd <- getLine
        let parts = words cmd
        if parts!!1 == "ls" then do
            conts <- ls
            genTree (Dir n conts)
        else do
            if parts!!2 == ".." then do
                return r
            else do
                let emptyNode = Dir (parts!!2) []
                newNode <- genTree emptyNode
                genTree (Dir n (newNode:l))

getSize:: Node -> Int
getSize (File _ s) = s
getSize (Dir _ l) = sum [getSize x | x <- l]

part1:: Node -> Int
part1 (File _ _) = 0
part1 r@(Dir _ l)
  | size <= 100000 = size + rest
  | otherwise      = rest
  where
    size = getSize r
    rest = sum [part1 x | x <- l]

part2:: Node -> Int -> Int
part2 (File _ _) _ = 999999999999999999
part2 r@(Dir _ l) m
  | size >= m = min size rest
  | otherwise = rest
  where
    size = getSize r
    rest = minimum [part2 x m | x <- l]

main :: IO ()
main = do
    _ <- getLine
    tree <- genTree (Dir "/" [])
    print tree
    let size = getSize tree
    print $ part2 tree (size - 40000000)


    
