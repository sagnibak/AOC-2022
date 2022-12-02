module Main where

main :: IO ()
main = do
  file_lines <- readFile "../inputs/day1.input"
  let input = parse $ lines file_lines
  let part1_out = show $ part1 input
  let part2_out = show $ part2 input
  putStrLn ("Part 1: " ++ part1_out)
  putStrLn ("Part 2: " ++ part2_out)

part1 :: [[Int]] -> Int
part1 = foldl max 0 . map sum

part2 :: [[Int]] -> Int
part2 = sumTh . foldl max3 (Th 0 0 0) . map sum

parse :: [String] -> [[Int]]
parse = parseHelper [[]]

parseHelper :: [[Int]] -> [String] -> [[Int]]
parseHelper acc [] = acc
parseHelper acc ("":xs) = parseHelper ([] : acc) xs
parseHelper (h:t) (x:xs) = parseHelper ((read x : h) : t) xs

data ThreeHeap = Th Int Int Int

max3 :: ThreeHeap -> Int -> ThreeHeap
max3 (Th a b c) x
  | x > a = Th x a b
  | x > b = Th a x b
  | x > c = Th a b x
  | otherwise = Th a b c


sumTh :: ThreeHeap -> Int
sumTh (Th a b c) = a + b + c