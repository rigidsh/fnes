module Main where


import System.Environment
import Data.Bits
import CPU6502

main :: IO()

main = do
    n <- fmap (\a -> (read (head a)) :: Int)  getArgs
    print $ run1Sec $ runProgramm cpu

run1Min :: [CPU6502] -> CPU6502
run1Min (x:xs) = if tick x < 60*1790000 then run1Min xs else x 

run1Sec :: [CPU6502] -> CPU6502
run1Sec (x:xs) = if tick x < 1790000 then run1Min xs else x 

runProgramm :: CPU6502 -> [CPU6502]
runProgramm cpu = nextItem:(runProgramm nextItem) where
    nextItem = iterateProcessor cpu

cpu :: CPU6502
cpu = cpuFromMemory [[42, 8, 9], [0xEA, 0xEA, 0x65, 2, 0x69, 1, 0x65, 0x00, 0x6D, 0x01, 0x00, 0xEA, 0xE8, 0xC8, 0xCA, 0x88, 0x90]]
