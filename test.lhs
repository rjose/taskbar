\begin{code}
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Timeline
import qualified Data.Time.Calendar as Calendar


foo :: Int -> (Int, Int)
foo x = (1, x)

t1 = defaultTask {name = Just "Task 1", duration = Just 2.0}
t2 = defaultTask {name = Just "Task 2", duration = Just 1.0}
t3 = defaultTask {name = Just "Task 3", duration = Just 3.0}
friday1 = Calendar.fromGregorian 2012 9 21
saturday1 = Calendar.fromGregorian 2012 9 22
sunday1 = Calendar.fromGregorian 2012 9 23

tasks = [t1, t2, t3]
weekdays = workdays workdayGenerator


main = defaultMain tests

tests = [
  testGroup "Next weekday" [
     testCase "Friday" ((head $ weekdays friday1) @?= Calendar.fromGregorian 2012 9 21),
     testCase "Saturday" ((head $ weekdays saturday1) @?= Calendar.fromGregorian 2012 9 24),
     testCase "Sunday" ((head $ weekdays sunday1) @?= Calendar.fromGregorian 2012 9 24)
     ]
  ]
\end{code}