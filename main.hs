main::IO()
main = do
    putStrLn "Current money:"
    currentMoney <- getLine
    putStrLn "Calculate for ____ time periods"
    months <- getLine
    putStrLn "With a gain per time period of ____ percent"
    gain <- getLine
    let result = mony (read months) (read currentMoney) (read gain)
    putStrLn $ show result

growth:: (Eq n, Fractional n) => n -> n
growth n = 1 + n / 100

mony::(Eq n, Fractional n) => n -> n -> n -> n
mony 0 m g = m
mony n m g = mony (n - 1) (m * growth g) g 