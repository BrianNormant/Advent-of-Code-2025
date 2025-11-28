module Main

import System
import System.File
import System.File.ReadWrite
import Lib
import Data.Either

import Solution

EXAMPLE_PATH : String
EXAMPLE_PATH = "./input.txt"
SOLUTION_PATH : String
SOLUTION_PATH = "./solution.txt"
INPUT_PATH : String
INPUT_PATH = "./input.txt"


readInput : IO (Maybe String)
readExample : IO (Maybe String)
readSolution : IO (Maybe String)
-- Run the program with the solution
-- If no cli are are given, use the example file
-- Otherwise, use the input file
-- When using the example file, the program status code will be
-- 0 if the solution match the expected solution
-- 1 otherwise
-- for any other error, the exit code will be 255
main : IO ()
main = do
  args <- getArgs
  if length args == 1
     then do
       putStrLn "Reading files"
       Just example <- readExample
        | Nothing => putStrLn "./example.txt not found or can't be read"
       Just solution <- readSolution
        | Nothing => putStrLn "./solution.txt not found or can't be read"
       putStrLn "Running example"
       putStrLn example
       putStrLn solution
     else do
       putStrLn "Reading files"
       Just input <- readInput
               | Nothing => putStrLn "./input.txt not found or can't be read"
       putStrLn "Running input"
       putStrLn input


readF : String -> IO (Maybe String)
readF file = withFile {io = IO}
      file Read
      (const $ pure Nothing)
      (fRead {io=IO} ||> map (mirror||> bimap pure id))
    |> map (either id (const Nothing))

readInput = readF INPUT_PATH

readExample = readF EXAMPLE_PATH

readSolution = readF SOLUTION_PATH
