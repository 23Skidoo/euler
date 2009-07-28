module Main
    where

-- This solution is probably too long, but it was a joy to write.

type Year         = Int
type Month        = Int
type DayOfMonth   = Int
type DayOfWeek    = Int
type Day          = (DayOfWeek, DayOfMonth, Month, Year)
type NumberOfDays = Int

isLeapYear :: Year -> Bool
isLeapYear y = if y `mod` 100 == 0
               then if y `mod` 400 == 0 then True else False
               else if y `mod` 4 == 0 then True else False

numDays :: Month -> Year -> NumberOfDays
numDays 2 y                     = if isLeapYear y then 29 else 28
numDays x _ | elem x [9,4,6,11] = 30
            | otherwise         = 31

nextMonth :: Month -> Month
nextMonth m = (m + 1) `mod` 12

turnCalendar :: Day -> Day
turnCalendar (dw, dm, m, y) = (ndw, ndm, nm, ny)
    where ndw = if dw + 1 == 8          then 1           else dw + 1
          ndm = if dm + 1 > numDays m y then 1           else dm + 1
          nm  = if ndm == 1             then nextMonth m else m
          ny  = if ndm == 1 && nm == 1  then y + 1       else y

daysFrom :: Day -> [Day]
daysFrom startDate = let nextDay = turnCalendar startDate
                     in startDate : daysFrom nextDay

-- 1 Jan 1901 was Tuesday
daysFrom1Jan1901 :: [Day]
daysFrom1Jan1901 = daysFrom (2,1,1,1901)

days20thCentury :: [Day]
days20thCentury = takeWhile (\(_,_,_,y) -> y < 2001) daysFrom1Jan1901

sundayFirst :: Day -> Bool
sundayFirst (7, 1, _, _) = True
sundayFirst _            = False

solve :: Int
solve = length . filter sundayFirst $ days20thCentury

main :: IO ()
main = print solve
