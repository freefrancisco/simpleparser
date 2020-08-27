import SimpleParser

main = do
    putStrLn "Enter String to parse:"
    str <- getLine
    let parsed = parseExpr str
    print parsed
    print $ evalExpr <$> parsed
