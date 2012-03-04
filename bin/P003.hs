import Primes

main = do
  let ans = head $ factors 600851475143
  putStrLn $ "Answer: " ++ show ans
