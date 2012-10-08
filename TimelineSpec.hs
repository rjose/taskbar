module TimelineSpec where

import Test.Hspec
import Timeline
import qualified Data.Time.Calendar as Calendar
import Work

-- Some sample days
monday1 = Calendar.fromGregorian 2012 9 17
friday1 = Calendar.fromGregorian 2012 9 21
saturday1 = Calendar.fromGregorian 2012 9 22
sunday1 = Calendar.fromGregorian 2012 9 23
monday2 = Calendar.fromGregorian 2012 9 24

-- Some sample vacation days
vacation1 = [friday1]

main = hspec $ do
  describe "days" $ do
    it "should start with the starting day" $
      (head $ days monday1) `shouldBe` monday1
      
  describe "isWeekend" $ do
    it "should know that saturday is a weekend" $
      isWeekend saturday1 `shouldBe` True
      
  describe "weekdays" $ do
    it "should start with a weekday" $
      (head $ weekdays monday1) `shouldBe` monday1
      
    it "should start with the next possible weekday" $
      (head $ weekdays saturday1) `shouldBe` monday2
      
  describe "scheduleWork" $ do
    it "should schedule work starting on the first available day" $
      let w = defaultWork {duration = Just 1.0}
          w' = scheduleWork (weekdays monday1) w
      in (range w') `shouldBe` Just (monday1, monday1)
         
    it "should schedule 5 days of work through Friday if starting on Monday" $
      let w = defaultWork {duration = Just 5.0}
          w' = scheduleWork (weekdays monday1) w
      in (range w') `shouldBe` Just (monday1, friday1)
             
  describe "scheduleWorklist" $ do
    it "should be able to schedule two pieces of work" $
      let w1 = defaultWork {duration = Just 5.0}
          w2 = defaultWork {duration = Just 1.0}
          scheduledWork = scheduleWorklist (weekdays monday1) [w1, w2]
          expectedRanges = map range scheduledWork
      in expectedRanges `shouldBe` [Just (monday1, friday1), Just (monday2, monday2)]