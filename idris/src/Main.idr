module Main

import Solution


main : IO ()
main = let tmp := Solution.sol "ignored"
        in putStrLn tmp
