import Primes

main = do
  putStrLn $ "10,001st prime: " ++ show (primes !! 10000)
