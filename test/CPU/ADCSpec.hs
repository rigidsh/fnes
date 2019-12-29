module CPU.ADCSpec (spec) where

import Test.Hspec
import CPU6502
import Memory

spec :: Spec
spec = do
    describe "IMM" $ do
        let cpu = iterateProcessor $ newCPU 
                $ (0x8000 <-> [0x69, 0x42]) -- ADC #$42
        it "check PC register" $ do (getPCRegister cpu) `shouldBe` (0x8000 + 2)
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x42
        it "check ticks" $ do (tick cpu) `shouldBe` 2
    describe "ZP" $ do
        let cpu = iterateProcessor $ newCPU 
                $ (0x0000 <-> [0x42])
                . (0x8000 <-> [0x65, 0x00]) -- ADC *00
        it "check PC register" $ do (getPCRegister cpu) `shouldBe` (0x8000 + 2)
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x42
        it "check ticks" $ do (tick cpu) `shouldBe` 3
    describe "ZPX" $ do
        let cpu = iterateProcessor 
                $ setXRegister 1 
                $ newCPU 
                $ (0x0000 <-> [0x00, 0x42])
                . (0x8000 <-> [0x75, 0x00]) -- ADC *00,X
        it "check PC register" $ do (getPCRegister cpu) `shouldBe` (0x8000 + 2)
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x42
        it "check ticks" $ do (tick cpu) `shouldBe` 4
    describe "ABS" $ do
        let cpu = iterateProcessor $ newCPU 
                $ (0x7900 <-> [0x42])
                . (0x8000 <-> [0x6D, 0x00, 0x79]) -- ADC 7900
        it "check PC register" $ do (getPCRegister cpu) `shouldBe` (0x8000 + 3)
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x42
        it "check ticks" $ do (tick cpu) `shouldBe` 4
    describe "ABSX" $ do
        let cpu = iterateProcessor $ setXRegister 1 $ newCPU 
                $ (0x7900 <-> [0x00, 0x42])
                . (0x8000 <-> [0x7D, 0x00, 0x79]) -- ADC 7900,X
        it "check PC register" $ do (getPCRegister cpu) `shouldBe` (0x8000 + 3)
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x42
        it "check ticks" $ do (tick cpu) `shouldBe` 4
    describe "ABSY" $ do
        let cpu = iterateProcessor 
                $ setYRegister 1 
                $ newCPU 
                $ (0x7900 <-> [0x00, 0x42])
                . (0x8000 <-> [0x79, 0x00, 0x79]) -- ADC 7900,Y
        it "check PC register" $ do (getPCRegister cpu) `shouldBe` (0x8000 + 3)
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x42
        it "check ticks" $ do (tick cpu) `shouldBe` 4
    describe "INDX" $ do
        let cpu = iterateProcessor 
                $ setXRegister 1 
                $ newCPU 
                $ (0x0000 <-> [0x00, 0x00, 0x01, 0x79])
                . (0x7900 <-> [0x00, 0x42])
                . (0x8000 <-> [0x61, 0x01]) -- ADC (1,X)
        it "check PC register" $ do (getPCRegister cpu) `shouldBe` (0x8000 + 2)
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x42
        it "check ticks" $ do (tick cpu) `shouldBe` 6 
    describe "INDY" $ do
        let cpu = iterateProcessor 
                $ setYRegister 1 
                $ newCPU 
                $ (0x0000 <-> [0x00, 0x00, 0x79])
                . (0x7900 <-> [0x00, 0x42])
                . (0x8000 <-> [0x71, 0x01]) -- ADC (1),Y
        it "check PC register" $ do (getPCRegister cpu) `shouldBe` (0x8000 + 2)
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x42
        it "check ticks" $ do (tick cpu) `shouldBe` 5 
    describe "0+0" $ do
        let cpu = iterateProcessor 
                $ setAccRegister 0x00
                $ setFlagC False 
                $ newCPU 
                $ (0x8000 <-> [0x69, 0x00])
        it "check flag C" $ do (getFlagC cpu) `shouldBe` False
        it "check flag Z" $ do (getFlagZ cpu) `shouldBe` True
        it "check flag N" $ do (getFlagN cpu) `shouldBe` False
        it "check flag V" $ do (getFlagV cpu) `shouldBe` False
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x00
    describe "0+0 with carry" $ do
        let cpu = iterateProcessor 
                $ setAccRegister 0x00
                $ setFlagC True 
                $ newCPU 
                $ (0x8000 <-> [0x69, 0x00])
        it "check flag C" $ do (getFlagC cpu) `shouldBe` False
        it "check flag Z" $ do (getFlagZ cpu) `shouldBe` False
        it "check flag N" $ do (getFlagN cpu) `shouldBe` False
        it "check flag V" $ do (getFlagV cpu) `shouldBe` False
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x01
    describe "2+2" $ do
        let cpu = iterateProcessor 
                $ setAccRegister 0x02
                $ setFlagC False 
                $ newCPU 
                $ (0x8000 <-> [0x69, 0x02])
        it "check flag C" $ do (getFlagC cpu) `shouldBe` False
        it "check flag Z" $ do (getFlagZ cpu) `shouldBe` False
        it "check flag N" $ do (getFlagN cpu) `shouldBe` False
        it "check flag V" $ do (getFlagV cpu) `shouldBe` False
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x04
    describe "2+2 with carry" $ do
        let cpu = iterateProcessor 
                $ setAccRegister 0x02
                $ setFlagC True 
                $ newCPU 
                $ (0x8000 <-> [0x69, 0x02])
        it "check flag C" $ do (getFlagC cpu) `shouldBe` False
        it "check flag Z" $ do (getFlagZ cpu) `shouldBe` False
        it "check flag N" $ do (getFlagN cpu) `shouldBe` False
        it "check flag V" $ do (getFlagV cpu) `shouldBe` False
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x05
    describe "0x01+0xFF" $ do
        let cpu = iterateProcessor 
                $ setAccRegister 0x01
                $ setFlagC False
                $ newCPU 
                $ (0x8000 <-> [0x69, 0xFF])
        it "check flag C" $ do (getFlagC cpu) `shouldBe` True
        it "check flag Z" $ do (getFlagZ cpu) `shouldBe` True
        it "check flag N" $ do (getFlagN cpu) `shouldBe` False
        it "check flag V" $ do (getFlagV cpu) `shouldBe` False
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x00
    describe "0x00+0xFF with carry" $ do
        let cpu = iterateProcessor 
                $ setAccRegister 0x00
                $ setFlagC True
                $ newCPU 
                $ (0x8000 <-> [0x69, 0xFF])
        it "check flag C" $ do (getFlagC cpu) `shouldBe` True
        it "check flag Z" $ do (getFlagZ cpu) `shouldBe` True
        it "check flag N" $ do (getFlagN cpu) `shouldBe` False
        it "check flag V" $ do (getFlagV cpu) `shouldBe` False
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x00
    describe "0x00+0x7F" $ do
        let cpu = iterateProcessor 
                $ setAccRegister 0x00
                $ setFlagC False
                $ newCPU 
                $ (0x8000 <-> [0x69, 0x7F])
        it "check flag C" $ do (getFlagC cpu) `shouldBe` False
        it "check flag Z" $ do (getFlagZ cpu) `shouldBe` False
        it "check flag N" $ do (getFlagN cpu) `shouldBe` False
        it "check flag V" $ do (getFlagV cpu) `shouldBe` False
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x7F
    describe "0x00+0x7F with carry" $ do
        let cpu = iterateProcessor 
                $ setAccRegister 0x00
                $ setFlagC True 
                $ newCPU 
                $ (0x8000 <-> [0x69, 0x7F])
        it "check flag C" $ do (getFlagC cpu) `shouldBe` False
        it "check flag Z" $ do (getFlagZ cpu) `shouldBe` False
        it "check flag N" $ do (getFlagN cpu) `shouldBe` True 
        it "check flag V" $ do (getFlagV cpu) `shouldBe` True 
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x80
    describe "0x01+0x7F" $ do
        let cpu = iterateProcessor 
                $ setAccRegister 0x01
                $ setFlagC False
                $ newCPU 
                $ (0x8000 <-> [0x69, 0x7F])
        it "check flag C" $ do (getFlagC cpu) `shouldBe` False
        it "check flag Z" $ do (getFlagZ cpu) `shouldBe` False
        it "check flag N" $ do (getFlagN cpu) `shouldBe` True 
        it "check flag V" $ do (getFlagV cpu) `shouldBe` True 
        it "check ACC register" $ do (getAccRegister cpu) `shouldBe` 0x80


