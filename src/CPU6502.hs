{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CPU6502 where

import System.Environment
import Data.Bits
import Memory


data CPU6502 = CPU6502 { 
    memory :: ListMemory
    , tick :: Int
    , registerACC:: Int
    , registerPC :: Int
    , registerS :: Int
    , registerX :: Int
    , registerY :: Int
    , flagC :: Bool
    , flagZ :: Bool
    , flagN :: Bool
    , flagV :: Bool
    , flagI :: Bool
    , flagD :: Bool} deriving Show


cpuFromMemory :: ListMemory -> CPU6502
cpuFromMemory memory = CPU6502 { 
    memory = memory, 
    tick = 0,
    registerACC = 0,
    registerPC = 0x8000,
    registerS = 0,
    registerX = 0,
    registerY = 0,
    flagC = False,
    flagZ = False,
    flagN = False,
    flagV = False,
    flagI = False,
    flagD = False}

newCPU :: MemoryBuilder -> CPU6502
newCPU memoryBuilder = cpuFromMemory $ memoryBuilder []

addTicks :: Int -> CPU6502 -> CPU6502
addTicks tick cpu@CPU6502{tick = oldValue} = cpu{tick = oldValue + tick}

getAccRegister :: CPU6502 -> Int 
getAccRegister = registerACC 
setAccRegister :: Int -> CPU6502 -> CPU6502
setAccRegister value = updateAccRegister (\_ -> value)
updateAccRegister :: (Int -> Int) -> CPU6502 -> CPU6502 
updateAccRegister updateFunc cpu@CPU6502{registerACC = accValue} = cpu { registerACC = updateFunc accValue}

getPCRegister :: CPU6502 -> Int 
getPCRegister = registerPC 
setPCRegister :: Int -> CPU6502 -> CPU6502
setPCRegister value cpu = updatePCRegister (\_ -> value) cpu
updatePCRegister :: (Int -> Int) -> CPU6502 -> CPU6502 
updatePCRegister updateFunc cpu@CPU6502{registerPC = pcValue} = cpu { registerPC = updateFunc pcValue}

readNextWord8 :: CPU6502 -> (Int, CPU6502)
readNextWord8 cpu = (read8 pc cpu, updatePCRegister (1+) cpu) where 
    pc = getPCRegister cpu

readNextWord16 :: CPU6502 -> (Int, CPU6502)
readNextWord16 cpu = (valueFrom16 (read16 pc cpu), updatePCRegister (2+) cpu) where 
    pc = getPCRegister cpu

getXRegister :: CPU6502 -> Int 
getXRegister = registerX
setXRegister :: Int -> CPU6502 -> CPU6502
setXRegister value cpu = updateXRegister (\_ -> value) cpu
updateXRegister :: (Int -> Int) -> CPU6502 -> CPU6502 
updateXRegister updateFunc cpu@CPU6502{registerX = xValue} = cpu { registerX = updateFunc xValue}

getYRegister :: CPU6502 -> Int 
getYRegister = registerY 
setYRegister :: Int -> CPU6502 -> CPU6502
setYRegister value cpu = updateYRegister (\_ -> value) cpu
updateYRegister :: (Int -> Int) -> CPU6502 -> CPU6502 
updateYRegister updateFunc cpu@CPU6502{registerY = yValue} = cpu { registerY = updateFunc yValue}

getFlagC :: CPU6502 -> Bool
getFlagC = flagC
setFlagC :: Bool -> CPU6502 -> CPU6502
setFlagC value cpu = cpu{flagC = value}

getFlagZ :: CPU6502 -> Bool
getFlagZ = flagZ
setFlagZ :: Bool -> CPU6502 -> CPU6502
setFlagZ value cpu = cpu{flagZ = value}

getFlagN :: CPU6502 -> Bool
getFlagN = flagN
setFlagN :: Bool -> CPU6502 -> CPU6502
setFlagN value cpu = cpu{flagN = value}

getFlagV :: CPU6502 -> Bool
getFlagV = flagV
setFlagV :: Bool -> CPU6502 -> CPU6502
setFlagV value cpu = cpu{flagV = value}

getFlagI :: CPU6502 -> Bool
getFlagI = flagI
setFlagI :: Bool -> CPU6502 -> CPU6502
setFlagI value cpu = cpu{flagI = value}

getFlagD :: CPU6502 -> Bool
getFlagD = flagV
setFlagD :: Bool -> CPU6502 -> CPU6502
setFlagD value cpu = cpu{flagD = value}

instance Memory CPU6502 where
    read8 address cpu  = read8 address (memory cpu) 
    read16 address cpu  = read16 address (memory cpu) 
    write8 address value cpu = cpu { memory = write8 address value (memory cpu)}
    write16 address value cpu = cpu { memory = write16 address value (memory cpu)} 

type CPUOperation = CPU6502 -> CPU6502 

resetCPU :: CPUOperation 
resetCPU  = cpuFromMemory.memory 

iterateProcessor :: CPUOperation 
iterateProcessor cpu = decodeNextOperation command newCPU
    where (command, newCPU) = readNextWord8 cpu

decodeNextOperation :: Int -> CPUOperation
decodeNextOperation command = 
    case command of
        0x65 -> adc.argZP.(addTicks 3)
        0x75 -> adc.argZPX.(addTicks 4)
        0x6D -> adc.argABS.(addTicks 4)
        0x7D -> adc.argABSX.(addTicks 4)
        0x79 -> adc.argABSY.(addTicks 4)
        0x69 -> adc.argIMM.(addTicks 2)
        0x61 -> adc.argINDX.(addTicks 6)
        0x71 -> adc.argINDY.(addTicks 5)
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




data MemoryAccessor = MemoryAccessor Int CPU6502 | ConstMemoryAccessor Int | NOPMemoryAccessor
readValue :: MemoryAccessor -> Int
readValue (ConstMemoryAccessor value) = value 
readValue (MemoryAccessor address cpu) = read8 address cpu
writeValue :: MemoryAccessor -> Int -> CPU6502
writeValue (MemoryAccessor address cpu) value = write8 address value cpu

instance Show MemoryAccessor where
    show = show.readValue 


argNOP :: CPU6502 -> (MemoryAccessor, CPU6502)
argNOP cpu = (NOPMemoryAccessor, cpu)

argZP :: CPU6502 -> (MemoryAccessor, CPU6502)
argZP cpu = ( MemoryAccessor arg newCPU, newCPU) where
    (arg, newCPU) = readNextWord8 cpu

argZPX :: CPU6502 -> (MemoryAccessor, CPU6502)
argZPX cpu = ( MemoryAccessor argAddress newCPU, newCPU) where
    argAddress = getXRegister newCPU  + arg
    (arg, newCPU) = readNextWord8 cpu

argABS :: CPU6502 -> (MemoryAccessor, CPU6502)
argABS cpu = ( MemoryAccessor arg newCPU, newCPU) where
    (arg, newCPU) = readNextWord16 cpu

argABSX :: CPU6502 -> (MemoryAccessor, CPU6502)
argABSX cpu = ( MemoryAccessor argAddress newCPU, newCPU) where
    argAddress =  getXRegister newCPU + arg
    (arg, newCPU) = readNextWord16 cpu

argABSY :: CPU6502 -> (MemoryAccessor, CPU6502)
argABSY cpu = ( MemoryAccessor argAddress newCPU, newCPU) where
    argAddress =  getYRegister newCPU + arg 
    (arg, newCPU) = readNextWord16 cpu

argINDX :: CPU6502 -> (MemoryAccessor, CPU6502)
argINDX cpu = ( MemoryAccessor argAddress newCPU, newCPU) where
    indAddress = getXRegister newCPU + arg
    argAddress = valueFrom16 $ read16 indAddress newCPU 
    (arg, newCPU) = readNextWord8 cpu

argINDY :: CPU6502 -> (MemoryAccessor, CPU6502)
argINDY cpu = ( MemoryAccessor argAddress newCPU, newCPU) where
    argAddress = getYRegister newCPU +  (valueFrom16 $ read16 arg newCPU) 
    (arg, newCPU) = readNextWord8 cpu

argIMM :: CPU6502 -> (MemoryAccessor, CPU6502)
argIMM cpu = (MemoryAccessor ((getPCRegister cpu)) cpu, updatePCRegister (1+) cpu)

argREL :: CPU6502 -> (MemoryAccessor, CPU6502)
argREL cpu = (ConstMemoryAccessor (toSignedByte arg), newCPU) where
    (arg, newCPU) = readNextWord8 cpu

toSignedByte :: Int -> Int
toSignedByte x 
    | x < 128 = x
    | otherwise = x - 0xFF - 1

valueFrom16 :: (Int, Int) -> Int
valueFrom16 (b2, b1) = b2*2^8 + b1

adc :: (MemoryAccessor, CPU6502) -> CPU6502 
adc (arg, cpu) = 
     ((setFlagC newCarryFlagValue)
     .(setFlagZ newZeroFlagValue)
     .(setFlagN newSignFlagValue)
     .(setFlagV newOverflowFlagValue)
     .(setAccRegister result)) cpu where
        accValue = getAccRegister cpu
        argValue = readValue arg
        carryFlagValue = getFlagC cpu
        sum = accValue + argValue + if carryFlagValue then 1 else 0
        newCarryFlagValue = if sum > 0xFF then True else False
        newZeroFlagValue = if result == 0 then True else False
        newSignFlagValue = if result > 0x7F then True else False
        newOverflowFlagValue = if (((accValue `xor`argValue) .&. 0x80) == 0) && (((accValue `xor` result) .&. 0x80) /= 0) then True else False
        result  = if newCarryFlagValue then sum - 0x100 else sum

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
