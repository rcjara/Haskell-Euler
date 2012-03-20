import Problems.P011
import Grid (readGrid)

main = do
  content <- getContents
  let ans = solve $ readGrid content
  putStrLn $ "Maximum for grid: " ++ show ans
