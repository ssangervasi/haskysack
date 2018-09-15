module Lib.RFC2550 (
  RFC2550(..)
  , epoch
  , fromList
) where

import Data.List

{-
https://codegolf.stackexchange.com/questions/80926/compute-the-rfc-2550-timestamp

RFC 2550 is a satirical proposal (published on April 1, 1999) for a space-efficient ASCII representation of timestamps that can support any date (even those prior to the beginning of the universe and those past the predicted end of the universe). The algorithm for computing a RFC 2550-compliant timestamp is as follows (note: all ranges include the start but exclude the end - 0 to 10,000 means all n where 0 <= n < 10000):

Year format
  
  Years 0 to 10,000: a 4-digit decimal number, left-padded with zeroes.

  Years 10,000 to 100,000: a 5-digit decimal number, prefixed with the character A.

  Years 100,000 to 10^30: the decimal number for the year, prefixed with the uppercase ASCII letter whose index in the English alphabet is equal to the number of digits in the decimal year, minus 5 (B for 6-digit years, C for 7-digit years, etc.).

  Years 10^30 to 10^56: the same format as 10,000 to 10^30, starting the letters over with A, and additionally prefixing a caret (^) to the string (so the year 10^30 is represented by ^A1000000000000000000000000000000, and the year 1031 is represented by ^B10000000000000000000000000000000).

  Years 10^56 to 10732: the year is prefixed by two carets and two ASCII uppercase letters. The uppercase letters form a base-26 number representing the number of digits in the year, minus 57.

  Years 10^732 onwards: the same format for 10^56 to 10^732 is used, extending it by adding an additional caret and uppercase letter when necessary.

  BCE years (prior to Year 0): compute the year string of the absolute value of the year. Then, replace all letters by their base-26 complement (A <-> Z, B <-> Y, etc.), replace all digits by their base-10 complement (0 <-> 9, 1 <-> 8, etc.), and replace carets with exclamation marks (!). If the year string is 4 digits or less (i.e. -1 to -10,000), prepend a forward slash (/). If the year string is not prefixed by a forward slash or an exclamation mark, prepend an asterisk (*).

Months, days, hours, minutes, and seconds: since these values are only ever 2 digits at the most, they are simply appended to the right of the year string, in decreasing order of significance, left-padded with zeroes if necessary to form 2-digit strings.

Additional precision: if additional precision (in the form of milliseconds, microseconds, nanoseconds, etc.) is needed, those values are left-padded with zeroes to 3 digits (because each value is 1/1000 of the previous value, and thus is at most 999) and appended to the end of the timestamp, in decreasing order of significance.
This format has the benefit of lexical sorting being equivalent to numeric sorting of the corresponding timestamp - if time A comes before time B, then the timestamp for A will come before the timestamp for B when lexical sorting is applied.


The challenge

Given an arbitrarily-long list of numeric values (corresponding to time values in decreasing order of significance, e.g. [year, month, day, hour, minute, second, millisecond]), output the corresponding RFC 2550 timestamp.


Rules

Solutions must work for any given input. The only limitations should be time and available memory.
Input may be taken in any reasonable, convenient format (such as a list of numerics, a list of strings, a string delimited by a single non-digit character, etc.).
The input will always contain at least one value (the year). Additional values are always in decreasing order of significance (e.g. the input will never contain a day value without a month value, or a second value followed by a month value).
Input will always be a valid time (e.g. there won't be any timestamps for February 30th).
Builtins which compute RFC 2550 timestamps are forbidden.


Examples

These examples use input as a single string, with the individual values separated by periods (.).

  1000.12.31.13.45.16.8 -> 10001231134516008
  
  12.1.5.1 -> 0012010501
  
  45941 -> A45941
  
  8675309.11.16 -> C86753091116
  
  47883552573911529811831375872990.1.1.2.3.5.8.13 -> ^B478835525739115298118313758729900101020305008013
  
  4052107100422150625478207675901330514555829957419806023121389455865117429470888094459661251.2.3.5.7.11 -> ^^BI40521071004221506254782076759013305145558299574198060231213894558651174294708880944596612510203050711
  
  -696443266.1.3.6.10.15.21.28 -> *V3035567330103061015021028
  
  -5342 -> /4657
  
  -4458159579886412234725624633605648497202 -> !Q5541840420113587765274375366394351502797
-}

listToTimestamp :: [Integer] -> String
listToTimestamp [] = formatYear 0

formatYear :: Integer  -> String
formatYear y = zeroPad y 4

zeroPad :: Integer -> Integer -> String
zeroPad n minLen = leftPad minLen '0' (show n)

leftPad :: Integer -> Char -> String -> String
leftPad minLen padChar toPad = padding ++ toPad
  where
    lenDiff = minLen - (genericLength toPad)
    padding = [padChar | _ <- [0..lenDiff]]


data RFC2550 = RFC2550
  { year :: Integer
  , month :: Integer
  , day :: Integer
  , hour :: Integer
  , minute :: Integer
  , second :: Integer
  , thousandths :: [Integer]
  } deriving (Show, Eq, Ord)

epoch = RFC2550
  { year = 0
  , month = 0
  , day = 0
  , hour = 0
  , minute = 0
  , second = 0
  , thousandths = []
  }

fromList :: [Integer] -> RFC2550
fromList [] = epoch
fromList (y:parts) = epoch { year = y }



