--Real World Haskell
--ch.01: A simple program
main = interact wordCount
 where wordCount input = show (length (lines input)) ++ "\n"