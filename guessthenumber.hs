
guessthenumber :: Int -> IO ()
guessthenumber value = do
    putStrLn "What number am I thinking?"
    putStr "Enter a number: "
    q <- getLine
    if  (read q :: Int) == value
    then putStrLn "You Win!"
    else do
        let (say, continue) =  (check value (read q :: Int))
	putStrLn say
        if continue then (guessthenumber value) else putStrLn "End"

check :: Int -> Int -> (String, Bool)
check value number = if number < value
    then ("It's bigger!", True)
    else ("It's smaller!", True)