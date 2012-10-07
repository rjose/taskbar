module Timeline
       where

import Data.Maybe
import Data.Foldable
import qualified Data.List as List
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Calendar.OrdinalDate as OrdinalDate

data Task = Task {
  name :: Maybe String,
  duration :: Maybe Double, -- Days
  range :: Maybe (Calendar.Day, Calendar.Day),  -- Start and end dates
  group :: Maybe String
  } 
          deriving (Show)
                   
defaultTask = Task {name = Nothing, 
                    duration = Nothing, 
                    range = Nothing,
                    group = Nothing}
              

-- TODO: Workdays should be different for each person
-- workdays :: Calendar.Day -> [Calendar.Day]

-- TODO: Consider renaming this to weekdayGenerator
-- Returns the next workday after the given day
workdayGenerator :: Calendar.Day -> Calendar.Day
workdayGenerator day | isWeekend nextDay = workdayGenerator nextDay
                     | otherwise         = nextDay
  where 
    nextDay = Calendar.addDays 1 day
    isWeekend d = (snd $ OrdinalDate.mondayStartWeek d) >= 6

workdaysStep :: (Calendar.Day -> Calendar.Day) -> Calendar.Day -> a -> (Calendar.Day, Calendar.Day)
workdaysStep generator curDay _ = (nextDay, nextDay)
  where nextDay = generator curDay


workdays :: (Calendar.Day -> Calendar.Day) -> Calendar.Day -> [Calendar.Day]
workdays generator start = result
  where
    (_, result) = List.mapAccumL (workdaysStep generator) start [1..]


updateTaskRange :: (Calendar.Day -> [Calendar.Day]) -> Calendar.Day -> Task -> Task
updateTaskRange resourceWorkdays start task
  | isJust taskDuration = task {range = Just (start, end)}
  | otherwise = task
  where
    taskDuration = duration task
    numWorkdaysMore = (round $ fromMaybe 0.0 taskDuration)
    end | numWorkdaysMore == 1 = start
        | otherwise            = last $ take numWorkdaysMore $ resourceWorkdays start

    
updateRangeStep :: (Calendar.Day -> [Calendar.Day]) -> Calendar.Day -> Task -> (Calendar.Day, Task)
updateRangeStep resourceWorkdays curDay task = (curDay', task')
  where task' = updateTaskRange resourceWorkdays curDay task
        curDay' = if isNothing $ range task'
                  then curDay
                  else last $ take 2 $ resourceWorkdays curDay


layOutTasks :: (Calendar.Day -> [Calendar.Day]) -> Calendar.Day -> [Task] -> [Task]
layOutTasks resourceWorkdays start tasks = tasks'
  where (_, tasks') = List.mapAccumL (updateRangeStep resourceWorkdays) start tasks


