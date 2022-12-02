module Main where


main :: IO ()
main = do
  file_lines <- readFile "../inputs/day2.input"
  let input = parse $ lines file_lines
  let part1_out = show $ part1 input
  let part2_out = show $ part1 $ parse2 $ lines file_lines
  putStrLn ("Part 1: " ++ part1_out)
  putStrLn ("Part 2: " ++ part2_out)


part1 :: [(Move, Move)] -> Int
part1 input = sum
    $ map (uncurry (+))
    $ bimap getMoveScore getOutcomeScore 
    $ zip (map snd input) (map getOutcome input)

data Outcome = OpponentWin | MeWin | Tie

data Move = Rock | Paper | Scissors deriving (Show)

bimap :: (a -> b) -> (c -> d) -> [(a, c)] -> [(b, d)]
bimap f g [] = []
bimap f g ((a, c) : xs) = (f a, g c) : bimap f g xs

getMoveScore :: Move -> Int
getMoveScore Rock = 1
getMoveScore Paper = 2
getMoveScore Scissors = 3

getOutcomeScore :: Outcome -> Int
getOutcomeScore OpponentWin = 0
getOutcomeScore MeWin = 6
getOutcomeScore Tie = 3

getOutcome :: (Move, Move) -> Outcome
getOutcome (Rock, Paper) = MeWin
getOutcome (Rock, Scissors) = OpponentWin
getOutcome (Rock, Rock) = Tie
getOutcome (Paper, Paper) = Tie
getOutcome (Paper, Scissors) = MeWin
getOutcome (Paper, Rock) = OpponentWin
getOutcome (Scissors, Paper) = OpponentWin
getOutcome (Scissors, Scissors) = Tie
getOutcome (Scissors, Rock) = MeWin

parse :: [String] -> [(Move, Move)]
parse = map parseLine

parse2 :: [String] -> [(Move, Move)]
parse2 = map parseLine2

parseLine :: String -> (Move, Move)
parseLine [a, _, c] = (parseChar a, parseChar c)

parseLine2 :: String -> (Move, Move)
parseLine2 [a, _, c] = (parseFirstChar a, getDesiredMove (parseFirstChar a) (parseSecondChar c))

parseChar :: Char -> Move
parseChar 'A' = Rock
parseChar 'X' = Rock
parseChar 'B' = Paper
parseChar 'Y' = Paper
parseChar 'C' = Scissors
parseChar 'Z' = Scissors

parseFirstChar :: Char -> Move
parseFirstChar 'A' = Rock
parseFirstChar 'B' = Paper
parseFirstChar 'C' = Scissors

parseSecondChar :: Char -> Outcome
parseSecondChar 'X' = OpponentWin
parseSecondChar 'Y' = Tie
parseSecondChar 'Z' = MeWin

getDesiredMove :: Move -> Outcome -> Move
getDesiredMove Rock OpponentWin = Scissors
getDesiredMove Rock Tie = Rock
getDesiredMove Rock MeWin = Paper
getDesiredMove Paper OpponentWin = Rock
getDesiredMove Paper Tie = Paper
getDesiredMove Paper MeWin = Scissors
getDesiredMove Scissors OpponentWin = Paper
getDesiredMove Scissors Tie = Scissors
getDesiredMove Scissors MeWin = Rock
