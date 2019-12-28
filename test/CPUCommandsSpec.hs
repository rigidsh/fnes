module CPUCommandsSpec (spec) where

import Test.Hspec
import CPU6502
import Memory

spec :: Spec
spec = do
    describe "ADC" $ do
        it "check IMM" $ do
            let cpu = iterateProcessor $ newCPU $ (0x8000 <-> [0x69, 0x42])
            (getPCRegister cpu) `shouldBe` (0x8000 + 2)
            (getAccRegister cpu) `shouldBe` 0x42
            (tick cpu) `shouldBe` 2
        it "check ZP" $ do
            let cpu = iterateProcessor $ newCPU $ (0x0000 <-> [0x42])
                                                 .(0x8000 <-> [0x65, 0x00]) 
            (getPCRegister cpu) `shouldBe` (0x8000 + 2)
            (getAccRegister cpu) `shouldBe` 0x42
            (tick cpu) `shouldBe` 3
        it "check ZPX" $ do
            let cpu = iterateProcessor $ newCPU $ (0x0000 <-> [0x42])
                                                 .(0x8000 <-> [0x75, 0x00]) 
            (getPCRegister cpu) `shouldBe` (0x8000 + 2)
            (getAccRegister cpu) `shouldBe` 0x42
            (tick cpu) `shouldBe` 4
        it "check ABS" $ do
            let cpu = iterateProcessor $ newCPU $ (0x7900 <-> [0x42])
                                                 .(0x8000 <-> [0x6D, 0x00, 0x79]) 
            (getPCRegister cpu) `shouldBe` (0x8000 + 3)
            (getAccRegister cpu) `shouldBe` 0x42
            (tick cpu) `shouldBe` 4


