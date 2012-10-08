module Work where

import qualified Data.Time.Calendar as Calendar

type Range = (Calendar.Day, Calendar.Day)

data Work = Work
    { workId :: Maybe Int       -- ^ Work Id
    , name :: Maybe String      -- ^ Work name
    , duration :: Maybe Double  -- ^ Duration in workdays
    , range :: Maybe Range      -- ^ Start and end dates
    } 
    deriving (Show)
                   
defaultWork = Work
  { workId = Nothing 
  , name = Nothing 
  , duration = Nothing
  , range = Nothing
  }
