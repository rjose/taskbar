module WorkSpec where

import Test.Hspec
import Work
import qualified Data.Time.Calendar as Calendar

-- Some sample days
monday1 = Calendar.fromGregorian 2012 9 17
friday1 = Calendar.fromGregorian 2012 9 21
saturday1 = Calendar.fromGregorian 2012 9 22
sunday1 = Calendar.fromGregorian 2012 9 23
monday2 = Calendar.fromGregorian 2012 9 24


main = hspec $ do
  describe "Default Work" $ do
    it "should have a null name" $
      (name defaultWork) `shouldBe` Nothing
      
      
      