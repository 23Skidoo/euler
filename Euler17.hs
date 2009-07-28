module Main
    where

---

lengths = map length

fr1to9 = ["one", "two", "three", "four", "five", "six", "seven",
          "eight", "nine"]

fr1to9_lengths = lengths fr1to9

fr10to19 = ["ten", "eleven", "twelve", "thirteen", "fourteen",
            "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

fr10to19_lengths = lengths fr10to19

fr20to90 = ["twenty", "thirty", "forty", "fifty", "sixty",
            "seventy", "eighty", "ninety"]

fr20to90_lengths = lengths fr20to90

---

fr21to99_lengths = map (\x -> x*9 + s_f1t9) fr20to90_lengths
    where s_f1t9 = sum fr1to9_lengths

fr1to99_length = sum fr1to9_lengths + sum fr10to19_lengths +
                 sum fr20to90_lengths + sum fr21to99_lengths

fr1hto9h_lengths = (map ((+) lh) fr1to9_lengths)
    where lh = length "hundred"

frx01tox99_lengths x = x_hundred_and * 99 + fr1to99_length
    where x_hundred_and = length x + length "hundred" + length "and"

fr101to999_length = sum fr1hto9h_lengths + (sum $ map frx01tox99_lengths fr1to9)

total_number = fr1to99_length + fr101to999_length + length ("one" ++ "thousand")

---

main :: IO ()
main = print total_number