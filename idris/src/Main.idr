module Main

import System
import System.File
import System.File.ReadWrite
import System.Console.GetOpt
import Lib
import Data.Either

import Solution

import Derive.Prelude
%language ElabReflection

EXAMPLE_PATH : String
EXAMPLE_PATH = "./example.txt"
SOLUTION_PATH : String
SOLUTION_PATH = "./solution.txt"
SOLUTION_PATH2 : String
SOLUTION_PATH2 = "./solution2.txt"
INPUT_PATH : String
INPUT_PATH = "./input.txt"

data Flag = Validate | Part2

%runElab derive "Flag" [Show, Eq]

options : List (OptDescr Flag)
options = [
  MkOpt ['v'] ["validate"] (NoArg Validate) "Validate the input",
  MkOpt ['2'] ["part2"] (NoArg Part2) "Second puzzle of the day"
  ]

readInput : IO (Maybe String)
readExample : IO (Maybe String)
readSolution : Bool -> IO (Maybe String)
-- Run the program with the solution
-- If no cli are are given, use the example file
-- Otherwise, use the input file
-- When using the example file, the program status code will be
-- 0 if the solution match the expected solution
-- 1 otherwise
-- for any other error, the exit code will be 255
main : IO ()
main = let opts = getOpt Permute options !getArgs in
           if all (/= Validate) opts.options
              then do
                putStrLn "Reading files"
                Just example <- readExample
                | Nothing => putStrLn "./example.txt not found or can't be read"
                Just solution <- readSolution (any (== Part2) opts.options)
                | Nothing => putStrLn "./solution.txt not found or can't be read"
                putStrLn "Running example"
                ex <- pure $ (
                  if all (/= Part2) opts.options
                     then (Solution.sol example) ++ "\n"
                     else (Solution.sol2 example) ++ "\n"
                  )
                putStrLn (
                  if ex == solution then "Success"
                                    else "Wrong solution\n" ++
                                    "expected: " ++ solution ++
                                    "got: " ++ ex
                  )
             else do
               putStrLn "Reading files"
               Just input <- readInput
               | Nothing => putStrLn "./input.txt not found or can't be read"
               putStrLn "Running input"
               putStrLn (
                 if all (/= Part2) opts.options
                    then Solution.sol input
                    else Solution.sol2 input
                 )


readF : String -> IO (Maybe String)
readF file = withFile {io = IO}
      file Read
      (const $ pure Nothing)
      (fRead {io=IO} ||> map (mirror||> bimap pure id))
    |> map (either id (const Nothing))

readInput = readF INPUT_PATH

readExample = readF EXAMPLE_PATH

readSolution False = readF "./solution.txt"
readSolution True = readF "./solution2.txt"
