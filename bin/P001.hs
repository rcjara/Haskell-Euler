import Problems.P001

main = do
  putStrLn $ "Solving via filtering a finite list and then summing: " ++ show filtering
  putStrLn $ "Solving via creating an infinite list of multiples and then suming: " ++ show infList
  putStrLn $ "Solving via a list comprehension: " ++ show listComp
