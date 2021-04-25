module Test.RLECore (rlecore) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog, (===), forAll)

import RLECore

import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

rlecore :: Spec
rlecore = describe "RLECoreTest" $ do
    it "compress + decompress == original" $ hedgehog $ do
        xs <- forAll $ Gen.string (Range.linear 0 1000) Gen.ascii 
        (runLengthDecode.runLengthEncode) xs === xs