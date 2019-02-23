module Main where

import Data.Time
import Text.Printf

-- I waffle on the appropriate separator for metrics from message, so it's configurable here.
-- Space after the separator is built-in, space before is not.
separator = ":"
-- separator = " until"

data Goals = Beginning (Integer, Int, Int) String
           | End (Integer, Int, Int) String
           | Money Double Double String
           | Generic Integer Integer String String
           | Percent Double String
           | Section
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

-- Beginnings and endings must be treated differently
--
-- Endings run till the end of the last day, so there's an extra day (counting today)
--
-- Beginnings run till the end of the day preceding (because the countdown ends once the day has
-- begun).  We accomplish this by not counting "today".
showGoal today (Beginning date event) =
  let d     = tupleToDay date
      delta = diffDays d today

  in formatDays delta event

showGoal today (End date event) =
  let d     = tupleToDay date
      delta = 1 + diffDays d today

  in formatDays delta event

showGoal _ (Money current goal event) =
  printf "%9s%s %s" (formatMoney $ round $ goal - current) separator event

showGoal _ (Generic current goal units event) =
  printf "%4d %4s%s %s" (goal - current) units separator event

showGoal _ (Percent current event) =
  printf "%4.0f %%%s    %s" (100 * (1.0 - current)) separator event

showGoal _ Section =
  printf "%4s" ""

formatDays delta event =
  printf "%4d days%s %s" delta separator event

formatMoney m =
  let m' = reverse $ show m
  in '$':(reverse $ go m')
  where go a | length a < 3 = a
             | otherwise    = take 3 a ++ ',':(go $ drop 3 a)
