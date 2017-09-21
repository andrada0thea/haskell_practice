-- ch04/FixLines
fixLines :: String -> String
fixLines input = unlines (splitLines input)