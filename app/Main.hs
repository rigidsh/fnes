module Main where


import System.Environment
import Data.Bits
import CPU6502
import Memory

main :: IO()

main = do
    print $ registerACC $ run1Sec $ runProgramm cpu

run1Sec :: [CPU6502] -> CPU6502
run1Sec (x:xs) = if tick x < 1790000 then run1Sec xs else x 


run1Min :: [CPU6502] -> CPU6502
run1Min (x:xs) = if tick x < 60*1790000 then run1Min xs else x 

runProgramm :: CPU6502 -> [CPU6502]
runProgramm cpu = nextItem:(runProgramm nextItem) where
    nextItem = iterateProcessor cpu

cpu :: CPU6502
cpu = newCPU 
      $ (0x0000 <-> [42, 8, 9])
      . (0x8000 <-> [0x69, 0x01, 0xB8, 0x50, 0xFB])
