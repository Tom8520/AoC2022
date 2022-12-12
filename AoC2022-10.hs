part1:: Int -> Int -> IO Int
part1 c x = do
    input <- getLine

    if input == "END" then do
        return 0
    else do
        let w = words input

        if head w == "noop" then do
            let c' = c + 1
            
            let num = if mod (c'+20) 40 == 0 then c' * x else 0

            rest <- part1 c' x

            return (num + rest)
        else do
            let c' = c + 1

            let num = if mod (c'+20) 40 == 0 then c' * x else 0

            let x' = x + (read (last w) :: Int)

            let c'' = c' + 1

            let num' = if mod (c''+20) 40 == 0 then c'' * x' else 0

            rest <- part1 c'' x'

            return (num + num' + rest)

part2:: Int -> Int -> String -> IO String
part2 c x crt= do
    input <- getLine

    if input == "END" then do
        return crt
    else do
        let w = words input

        if head w == "noop" then do
            let c' = mod (c + 1) 40

            let crt' = crt ++ (if abs (x - c) <= 1 then "#" else ".")
            let crt'' = crt' ++ (if mod c' 40 == 0 then "\n" else "")

            part2 c' x crt''
        else do
            let c' = mod (c + 1) 40

            let crt' = crt ++ (if abs (x - c) <= 1 then "#" else ".")
            let crt'' = crt' ++ (if mod c' 40 == 0 then "\n" else "")

            let x' = x + (read (last w) :: Int)

            let c'' = mod (c' + 1) 40

            let crt''' = crt'' ++ (if abs (x - c') <= 1 then "#" else ".")
            let crt'''' = crt''' ++ (if mod c'' 40 == 0 then "\n" else "")

            part2 c'' x' crt''''

main:: IO ()
main = do
    crt <- part2 0 1 ""

    putStr crt

