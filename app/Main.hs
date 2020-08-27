import SimpleParser

main = do
    putStrLn "Enter String to parse:"
    str <- getLine
    let parsed = parseExpr str
    putStrLn "Parsing..."
    print parsed
    putStrLn "Evaluting..."
    print $ evalExpr <$> parsed

