module Timeline where

import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Calendar.OrdinalDate as OrdinalDate
import qualified Data.List as List
import Data.Maybe
import Work

days :: Calendar.Day -> [Calendar.Day]
days start = let dayAdder = flip Calendar.addDays start
             in map dayAdder [0..]
                
isWeekend :: Calendar.Day -> Bool
isWeekend day = let dayOfWeek = snd $ OrdinalDate.mondayStartWeek day
                    saturday = 6
                in dayOfWeek >= saturday
                   
weekdays :: Calendar.Day -> [Calendar.Day]
weekdays start = filter (\d -> not . isWeekend $ d) $ days start


scheduleWork :: [Calendar.Day]  -- ^ List of days when work can be scheduled
             -> Work            -- ^ Work to be scheduled
             -> Work            -- ^ Resulting Work with updated range
scheduleWork days work
  -- Assuming duration of 1d if not specified
  | isNothing $ duration work = work {range = Just (startDay, startDay)}  
  | otherwise = let numWorkdays = ceiling $ fromJust $ duration work
                    endDay = days !! (numWorkdays - 1)
                in work {range = Just (startDay, endDay)}
  where startDay = days !! 0


scheduleWorklist :: [Calendar.Day]  -- ^ List of days when work can be scheduled
                 -> [Work]          -- ^ Worklist to be scheduled
                 -> [Work]          -- ^ Scheduled work
scheduleWorklist days worklist = snd $ List.mapAccumL step days worklist
  where step dayList w = let w' = scheduleWork dayList w
                             endDay = snd . fromJust . range $ w'
                             dayList' = dropWhile (\d -> d <= endDay) dayList
                         in (dayList', w')