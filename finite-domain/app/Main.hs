module Main (main) where

-- import FD
import FDEx
import Control.Monad
import Sudoku

main :: IO ()
-- main = print runTest
main = do
    putStrLn "Start"
    -- printSudoku testSudokuKindaMid
    printSudoku testSudokuDMOverton
    -- printSudoku testSudoku_s15c

runTest :: [[Int]]
runTest = runFD test

test :: FD s [Int]
test = do
    x <- newVar [0..3]
    y <- newVar [0..3]
    ( x `lessThan` y) `mplus` (x `same` y)
    x `hasValue` 2
    labelling [x, y]


testSudokuDMOverton :: Puzzle
testSudokuDMOverton = [
    0, 0, 0, 0, 8, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 6, 5, 0, 7,
    4, 0, 2, 7, 0, 0, 0, 0, 0,
    0, 8, 0, 3, 0, 0, 1, 0, 0,
    0, 0, 3, 0, 0, 0, 8, 0, 0,
    0, 0, 5, 0, 0, 9, 0, 7, 0,
    0, 5, 0, 0, 0, 8, 0, 0, 6,
    3, 0, 1, 2, 0, 4, 0, 0, 0,
    0, 0, 6, 0, 1, 0, 0, 0, 0 ]

testSudokuKindaMid :: Puzzle
testSudokuKindaMid = [
    2, 0, 6, 0, 0, 0, 0, 4, 9, 
    0, 3, 7, 0, 0, 9, 0, 0, 0, 
    1, 0, 0, 7, 0, 0, 0, 0, 6, 
    0, 0, 0, 5, 8, 0, 9, 0, 0, 
    7, 0, 5, 0, 0, 0, 8, 0, 4, 
    0, 0, 9, 0, 6, 2, 0, 0, 0, 
    9, 0, 0, 0, 0, 4, 0, 0, 1, 
    0, 0, 0, 3, 0, 0, 4, 9, 0, 
    4, 1, 0, 0, 0, 0, 2, 0, 8 
    ]

-- http://lipas.uwasa.fi/~timan/sudoku/
testSudoku_s15c :: Puzzle
testSudoku_s15c = [
    7, 9, 0, 0, 0, 0, 0, 0, 3, 
    0, 0, 0, 0, 0, 0, 0, 6, 0, 
    8, 0, 1, 0, 0, 4, 0, 0, 2, 
    0, 0, 5, 0, 0, 0, 0, 0, 0, 
    3, 0, 0, 1, 0, 0, 0, 0, 0, 
    0, 4, 0, 0, 0, 6, 2, 0, 9, 
    2, 0, 0, 0, 3, 0, 0, 0, 6, 
    0, 3, 0, 6, 0, 5, 4, 2, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0
                  ]
