-- buildTimeline :: Task t => [t] -> d -> [d] -> [t]

import Data.Maybe
import Data.Foldable
import qualified Data.List as List
import qualified Data.Time.Calendar as Calendar

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
              
name' :: Task -> String
name' t = fromMaybe "<unnamed task>" $ name t


-- TODO: Need a function that computes non-workdays

updateRange :: Calendar.Day -> Task -> Task
updateRange start task 
  | isJust taskDuration = task {range = Just (start, end)}
  | otherwise  = task
  where 
    taskDuration = duration task
    numDays = (round $ fromMaybe 0.0 taskDuration) - 1 -- Need to handle non-workdays
    end = Calendar.addDays numDays start
    
updateRangeStep :: Calendar.Day -> Task -> (Calendar.Day, Task)
updateRangeStep curDay task = (curDay', task')
  where task' = updateRange curDay task
        curDay' = if isNothing $ range task'
                  then curDay
                  -- Need to handle going to next workday
                  else Calendar.addDays 1 $ snd (fromMaybe (curDay, curDay) (range task')) 


layOutTasks :: Calendar.Day -> [Task] -> [Task]
layOutTasks start tasks = tasks'
                                where (_, tasks') = List.mapAccumL updateRangeStep start tasks


t1 = defaultTask {name = Just "Task 1", duration = Just 2.0}
t2 = defaultTask {name = Just "Task 2", duration = Just 1.0}
t3 = defaultTask {name = Just "Task 3", duration = Just 3.0}
d = Calendar.fromGregorian 2012 9 22
tasks = [t1, t2, t3]