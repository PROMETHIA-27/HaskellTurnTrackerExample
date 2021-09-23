module Main where

type Order = [String]

remove _ [] = []
remove x (y:ys) | x == y = remove x ys
				| x /= y = y : remove x ys

pop (x:xs) = xs

printAll (x:[]) = putStrLn(x)
printAll (x:xs) = do
	putStrLn(x)
	printAll(xs)
printAll [] = putStrLn("No characters in turn order")

simulate :: Order -> IO ()
simulate ord = do
	input <- getLine
	act input ord

act :: String -> Order -> IO ()
act ('A':'d':'d':' ':name) ord = do
	putStrLn("Adding " ++ name)
	simulate (name:ord)
act ('R':'e':'m':'o':'v':'e':' ':name) ord = do
	putStrLn("Removing " ++ name)
	simulate (remove name ord)
act "Order" ord = do 
	printAll(ord)
	simulate ord
act "Turn" ord = do
	if length ord > 0 
		then do
			let first = ord !! 0
			putStrLn(first)
			simulate ((pop ord) ++ [first])
		else do
			putStrLn("No characters added! It is no one's turn.")
			simulate ord
act "Exit" ord = putStrLn("See you later!")
act _ ord = do
	putStrLn("Please enter a valid input. Valid commands: Add [Name], Remove [Name], Order, Turn, Exit.")
	simulate ord

main :: IO ()
main = simulate []
