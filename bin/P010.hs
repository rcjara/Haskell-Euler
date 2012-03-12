import Primes

main = do
  let ans = sum $ takeWhile (< 2000000) primes
  putStrLn $ "Total of sums less than 2,000,000: " ++ show ans
