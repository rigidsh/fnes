{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CPU6502 where

import System.Environment
import Data.Bits
import Memory


data Registers6502 = Registers6502 { 
    acc :: Int
    , pc :: Int
    , s :: Int
    , x :: Int
    , y :: Int
    , flagC :: Bool
    , flagZ :: Bool
    , flagN :: Bool
    , flagV :: Bool
    , flagI :: Bool
    , flagD :: Bool} deriving Show
data CPU6502 = CPU6502 { memory :: ListMemory, registers :: Registers6502, tick :: Int} deriving Show
defaultRegisters = Registers6502 {acc = 0, pc = 0x8000, s = 0, x = 0, y = 0, flagC = False, flagZ = False, flagN = False, flagV = False, flagI = False, flagD = False}

cpuFromMemory :: ListMemory -> CPU6502
cpuFromMemory memory = CPU6502 { memory = memory, registers = defaultRegisters, tick = 0 }

newCPU :: MemoryBuilder -> CPU6502
newCPU memoryBuilder = cpuFromMemory $ memoryBuilder []

addTicks :: Int -> CPU6502 -> CPU6502
addTicks tick cpu@CPU6502{tick = oldValue} = cpu{tick = oldValue + tick}

getAccRegister :: CPU6502 -> Int 
getAccRegister = acc.registers 
setAccRegister :: Int -> CPU6502 -> CPU6502
setAccRegister value = updateAccRegister (\_ -> value)
updateAccRegister :: (Int -> Int) -> CPU6502 -> CPU6502 
updateAccRegister updateFunc cpu@CPU6502{registers = registers@Registers6502{acc = accValue}} = cpu { registers = registers{acc = updateFunc accValue}}

getPCRegister :: CPU6502 -> Int 
getPCRegister = pc.registers 
setPCRegister :: Int -> CPU6502 -> CPU6502
setPCRegister value cpu = updatePCRegister (\_ -> value) cpu
updatePCRegister :: (Int -> Int) -> CPU6502 -> CPU6502 
updatePCRegister updateFunc cpu@CPU6502{registers = registers@Registers6502{pc = pcValue}} = cpu { registers = registers{pc = updateFunc pcValue}}

getXRegister :: CPU6502 -> Int 
getXRegister = x.registers
setXRegister :: Int -> CPU6502 -> CPU6502
setXRegister value cpu = updateXRegister (\_ -> value) cpu
updateXRegister :: (Int -> Int) -> CPU6502 -> CPU6502 
updateXRegister updateFunc cpu@CPU6502{registers = registers@Registers6502{x = xValue}} = cpu { registers = registers{x = updateFunc xValue}}

getYRegister :: CPU6502 -> Int 
getYRegister = y.registers 
setYRegister :: Int -> CPU6502 -> CPU6502
setYRegister value cpu = updateYRegister (\_ -> value) cpu
updateYRegister :: (Int -> Int) -> CPU6502 -> CPU6502 
updateYRegister updateFunc cpu@CPU6502{registers = registers@Registers6502{y = yValue}} = cpu { registers = registers{y = updateFunc yValue}}

getFlagC :: CPU6502 -> Bool
getFlagC = flagC.registers
setFlagC :: Bool -> CPU6502 -> CPU6502
setFlagC value cpu@CPU6502{registers = registers} = cpu{registers = registers{flagC = value}}

getFlagZ :: CPU6502 -> Bool
getFlagZ = flagZ.registers
setFlagZ :: Bool -> CPU6502 -> CPU6502
setFlagZ value cpu@CPU6502{registers = registers} = cpu{registers = registers{flagZ = value}}

getFlagN :: CPU6502 -> Bool
getFlagN = flagN.registers
setFlagN :: Bool -> CPU6502 -> CPU6502
setFlagN value cpu@CPU6502{registers = registers} = cpu{registers = registers{flagN = value}}

getFlagV :: CPU6502 -> Bool
getFlagV = flagV.registers
setFlagV :: Bool -> CPU6502 -> CPU6502
setFlagV value cpu@CPU6502{registers = registers} = cpu{registers = registers{flagV = value}}

getFlagI :: CPU6502 -> Bool
getFlagI = flagI.registers
setFlagI :: Bool -> CPU6502 -> CPU6502
setFlagI value cpu@CPU6502{registers = registers} = cpu{registers = registers{flagI = value}}

getFlagD :: CPU6502 -> Bool
getFlagD = flagV.registers
setFlagD :: Bool -> CPU6502 -> CPU6502
setFlagD value cpu@CPU6502{registers = registers} = cpu{registers = registers{flagD = value}}

instance Memory CPU6502 where
    read8 address cpu  = read8 address (memory cpu) 
    read16 address cpu  = read16 address (memory cpu) 
    write8 address value cpu = cpu { memory = write8 address value (memory cpu)}
    write16 address value cpu = cpu { memory = write16 address value (memory cpu)} 

type CPUOperation = CPU6502 -> CPU6502 

resetCPU :: CPUOperation 
resetCPU  = cpuFromMemory.memory 

iterateProcessor :: CPUOperation 
iterateProcessor cpu = decodeNextOperation cpu cpu

decodeNextOperation :: CPU6502 -> CPUOperation
decodeNextOperation cpu = 
    case command of
        0x65 -> adc.argZP.(addTicks 3)
        0x75 -> adc.argZPX.(addTicks 4)
        0x6D -> adc.argABS.(addTicks 4)
        0x7D -> adc.argABSX.(addTicks 4)
        0x79 -> adc.argABSY.(addTicks 4)
        0x69 -> adc.argIMM.(addTicks 2)
        0xEA -> nop.argNOP.(addTicks 2)
        0xE8 -> inx.argNOP.(addTicks 2)
        0xCA -> dex.argNOP.(addTicks 2)
        0xC8 -> iny.argNOP.(addTicks 2)
        0x88 -> dey.argNOP.(addTicks 2)
        0x90 -> bcc.argREL.(addTicks 2)
        0xB0 -> bcs.argREL.(addTicks 2)
        0xF0 -> beq.argREL.(addTicks 2)
        0x30 -> bmi.argREL.(addTicks 2)
        0xD0 -> bne.argREL.(addTicks 2)
        0x10 -> bpl.argREL.(addTicks 2)
        0x50 -> bvc.argREL.(addTicks 2)
        0x70 -> bvs.argREL.(addTicks 2)
        0x18 -> clc.argNOP.(addTicks 2)
        0xD8 -> cld.argNOP.(addTicks 2)
        0x58 -> cli.argNOP.(addTicks 2)
        0xB8 -> clv.argNOP.(addTicks 2)

        0x85 -> sta.argZP.(addTicks 3)
        0x95 -> sta.argZPX.(addTicks 4)
        0x8D -> sta.argABS.(addTicks 4)
        0x9D -> sta.argABSX.(addTicks 5)
        0x99 -> sta.argABSY.(addTicks 5)


    where command = read8 (getPCRegister cpu) cpu 

data MemoryAccessor = MemoryAccessor Int CPU6502 | ConstMemoryAccessor Int | NOPMemoryAccessor
readValue :: MemoryAccessor -> Int
readValue (ConstMemoryAccessor value) = value 
readValue (MemoryAccessor address cpu) = read8 address cpu
writeValue :: MemoryAccessor -> Int -> CPU6502
writeValue (MemoryAccessor address cpu) value = write8 address value cpu

instance Show MemoryAccessor where
    show = show.readValue 


argNOP :: CPU6502 -> (MemoryAccessor, CPU6502)
argNOP cpu = (NOPMemoryAccessor, updatePCRegister (1+) cpu)

argZP :: CPU6502 -> (MemoryAccessor, CPU6502)
argZP cpu = ( MemoryAccessor (read8 ( (getPCRegister cpu)  + 1)  cpu) cpu, updatePCRegister (2+) cpu) 

argZPX :: CPU6502 -> (MemoryAccessor, CPU6502)
argZPX cpu = ( MemoryAccessor argAddress cpu, updatePCRegister (2+) cpu) where
    argAddress = getXRegister cpu + ( read8 ((getPCRegister cpu) + 1) cpu)

argABS :: CPU6502 -> (MemoryAccessor, CPU6502)
argABS cpu = ( MemoryAccessor argAddress cpu, updatePCRegister (3+) cpu) where
    argAddress =  valueFrom16 (read16 ( (getPCRegister cpu)  + 1)  cpu)

argABSX :: CPU6502 -> (MemoryAccessor, CPU6502)
argABSX cpu = ( MemoryAccessor argAddress cpu, updatePCRegister (3+) cpu) where
    argAddress =  getXRegister cpu + valueFrom16 (read16 ( (getPCRegister cpu)  + 1)  cpu)

argABSY :: CPU6502 -> (MemoryAccessor, CPU6502)
argABSY cpu = ( MemoryAccessor argAddress cpu, updatePCRegister (3+) cpu) where
    argAddress =  getYRegister cpu + valueFrom16 (read16 ( (getPCRegister cpu)  + 1)  cpu)

argIMM :: CPU6502 -> (MemoryAccessor, CPU6502)
argIMM cpu = (MemoryAccessor ((getPCRegister cpu) + 1) cpu, updatePCRegister (2+) cpu)

argREL :: CPU6502 -> (MemoryAccessor, CPU6502)
argREL cpu = (ConstMemoryAccessor (toSignedByte (read8 ((getPCRegister cpu) +1) cpu)), updatePCRegister (2+) cpu) 

toSignedByte :: Int -> Int
toSignedByte x 
    | x < 128 = x
    | otherwise = x - 0xFF - 1

valueFrom16 :: (Int, Int) -> Int
valueFrom16 (b2, b1) = b2*2^8 + b1

adc :: (MemoryAccessor, CPU6502) -> CPU6502 
adc (arg, cpu) = updateAccRegister (\v -> v + (readValue arg)) cpu   

bcc :: (MemoryAccessor, CPU6502) -> CPU6502
bcc (arg, cpu) = if (getFlagC cpu) == False then setPCRegister ((getPCRegister cpu) + (readValue arg))  cpu else cpu

bcs :: (MemoryAccessor, CPU6502) -> CPU6502
bcs (arg, cpu) = if (getFlagC cpu) == True then setPCRegister ((getPCRegister cpu) + (readValue arg))  cpu else cpu

beq :: (MemoryAccessor, CPU6502) -> CPU6502
beq (arg, cpu) = if (getFlagZ cpu) == True then setPCRegister ((getPCRegister cpu) + (readValue arg))  cpu else cpu

bne :: (MemoryAccessor, CPU6502) -> CPU6502
bne (arg, cpu) = if (getFlagZ cpu) == False then setPCRegister ((getPCRegister cpu) + (readValue arg))  cpu else cpu

bmi :: (MemoryAccessor, CPU6502) -> CPU6502
bmi (arg, cpu) = if (getFlagN cpu) == True then setPCRegister ((getPCRegister cpu) + (readValue arg))  cpu else cpu

bpl :: (MemoryAccessor, CPU6502) -> CPU6502
bpl (arg, cpu) = if (getFlagN cpu) == False then setPCRegister ((getPCRegister cpu) + (readValue arg))  cpu else cpu

bvs :: (MemoryAccessor, CPU6502) -> CPU6502
bvs (arg, cpu) = if (getFlagV cpu) == True then setPCRegister ((getPCRegister cpu) + (readValue arg))  cpu else cpu

bvc :: (MemoryAccessor, CPU6502) -> CPU6502
bvc (arg, cpu) = if (getFlagV cpu) == False then setPCRegister ((getPCRegister cpu) + (readValue arg))  cpu else cpu

clc :: (MemoryAccessor, CPU6502) -> CPU6502
clc (_, cpu) = setFlagC False cpu

cld :: (MemoryAccessor, CPU6502) -> CPU6502
cld (_, cpu) = setFlagD False cpu

cli :: (MemoryAccessor, CPU6502) -> CPU6502
cli (_, cpu) = setFlagI False cpu

clv :: (MemoryAccessor, CPU6502) -> CPU6502
clv (_, cpu) = setFlagV False cpu

nop :: (MemoryAccessor, CPU6502) -> CPU6502 
nop (_, cpu) = cpu

inx :: (MemoryAccessor, CPU6502) -> CPU6502 
inx (_, cpu) = updateXRegister (1+) cpu

dex :: (MemoryAccessor, CPU6502) -> CPU6502 
dex (_, cpu) = updateXRegister (1-) cpu

iny :: (MemoryAccessor, CPU6502) -> CPU6502 
iny (_, cpu) = updateYRegister (1+) cpu

dey :: (MemoryAccessor, CPU6502) -> CPU6502 
dey (_, cpu) = updateYRegister (1-) cpu

-- S
sta :: (MemoryAccessor, CPU6502) -> CPU6502
sta (arg, cpu) = writeValue arg $ getAccRegister cpu
