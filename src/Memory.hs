{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Memory where

import System.Environment
import Data.Bits


class Memory a where
    read8 :: Int -> a -> Int
    read16 :: Int -> a -> (Int, Int)
    write8 :: Int -> Int -> a -> a
    write16 :: Int -> (Int, Int) -> a -> a

type ListMemory = [[Int]] 

type MemoryBuilder = ListMemory -> ListMemory 

(<->) :: Int -> [Int] -> MemoryBuilder 
(<->) address (s:xs) memory = write8 address s $ (<->) (address + 1) xs memory
(<->) address [] memory = memory

decodeAddress :: Int -> (Int, Int)
decodeAddress address = ( shiftR (address .&. 0xFF00) 8, address .&. 0xFF)

instance Memory ListMemory where
    read8 address memory = read8FromPage (getMemoryPage memory pageNumber) offset where 
        (pageNumber, offset) = decodeAddress address
    read16 address memory= read16FromPage (getMemoryPage memory pageNumber) offset where 
        (pageNumber, offset) = decodeAddress address
    write8 address value memory = updateMemoryPage memory pageNumber $ write8ToPage offset value where
        (pageNumber, offset) = decodeAddress address
    write16 address value memory = updateMemoryPage memory pageNumber $ write16ToPage offset value where
        (pageNumber, offset) = decodeAddress address

updateMemoryPage :: ListMemory -> Int -> ([Int] -> [Int]) -> ListMemory
updateMemoryPage ( (currentPage:restPages)) pageNumber updater
    | pageNumber == 0 =  ((updater currentPage):restPages)
    | otherwise =  (currentPage:t) where
         t = updateMemoryPage ( restPages) (pageNumber - 1) updater 
updateMemoryPage [] pageNumber updater
    | pageNumber == 0 =  [(updater emptyPage)]
    | otherwise = (emptyPage:t) where
         t = updateMemoryPage [] (pageNumber - 1) updater

emptyPage :: [Int]
emptyPage = take 256 (repeat 0)

getMemoryPage :: ListMemory -> Int -> [Int]
getMemoryPage [] _ = emptyPage 
getMemoryPage ( (firstPage:otherPages)) pageNumber 
    | pageNumber == 0 = firstPage
    | otherwise = getMemoryPage ( otherPages) (pageNumber - 1)

read8FromPage :: [Int] -> Int -> Int
read8FromPage page offset = page!!offset

-- TODO: optimize
read16FromPage :: [Int] -> Int -> (Int, Int)
read16FromPage page address = (read8FromPage page (address + 1), read8FromPage page address)

write8ToPage :: Int -> Int -> [Int] -> [Int]
write8ToPage offset value (x:xs) 
    | offset == 0 = value:xs
    | otherwise = x:write8ToPage (offset - 1) value xs
write8ToPage offset value []
    | offset == 0 = [value]
    | otherwise = 0:write8ToPage (offset - 1) value [] 

write16ToPage :: Int -> (Int, Int) -> [Int] -> [Int]
write16ToPage offset (b2, b1) page = write8ToPage (offset + 1) b2 $write8ToPage offset b1 page

