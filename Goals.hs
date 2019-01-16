module Main where

import Data.Time
import Text.Printf

data Goals = Countdown (Integer, Int, Int) String
  deriving ( Read )

main = do
  -- I use this from the command line (or bashrc) as:
  --   $ goals < ~/.goals
  --
  -- Where ~/.goals contains a Haskell-syntax list of Goals
  goals <- fmap read getContents

  today <- fmap utctDay getCurrentTime

  let messages = map (showGoal today) goals

  mapM_ putStrLn messages

tupleToDay (year, month, day) = fromGregorian year month day

showGoal today (Countdown date event) =
  let d     = tupleToDay date

      -- I usually want to know "days till the end of the day"
      -- The number of days till the end of today is ~1 day, so we add 1
      -- Otherwise, the number of days till the end of today is 0
      delta = 1 + diffDays d today

  in printf "%3d" delta ++ " days till " ++ event
