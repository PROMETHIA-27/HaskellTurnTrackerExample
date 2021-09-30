import Data.List

remove _ [] = []
remove x (y:ys) | x == y = remove x ys
				| x /= y = y : remove x ys

pop (x:xs) = xs

printAll (x:[]) = putStrLn(x)
printAll (x:xs) = do
	putStrLn(x)
	printAll(xs)
printAll [] = putStrLn("No characters in turn order")

simulate ord = do
	input <- getLine
	act input ord

act input ord | Just name <- stripPrefix "Add " input = do
			    putStrLn("Adding " ++ name)
			    simulate (name:ord)
			  | Just name <- stripPrefix "Remove " input = do
			    putStrLn("Removing " ++ name)
			    simulate (remove name ord)
			  | input == "Order" = do
			    printAll(ord)
			    simulate ord
			  | input == "Turn" = do
              if length ord > 0
                then do
                    let first = ord !! 0
                    putStrLn(first)
                    simulate((pop ord) ++ [first])
                else do
                    putStrLn("No characters available!")
                    simulate ord
			  | input == "Exit" = putStrLn("See you later!")
			  | otherwise = do
			    putStrLn("Please enter a valid input.")
			    simulate ord	

main = simulate []
