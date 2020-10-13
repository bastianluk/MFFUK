delka [] = 0
delka (x:xs) = 1 + delka xs

lichy x = mod x 2 == 1
lichySeznam xs = map lichy xs