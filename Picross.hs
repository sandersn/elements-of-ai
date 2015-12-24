import Control.Monad (mzero)
data Pix = Black | White | Unknown deriving (Show, Eq)
data Axis = Horizontal | Vertical
type Pattern = [Specifier]
type Row = [Pix]
type Puzzle = [Row] -- TODO: probably not right
data Specifier = Star | Plus | Num Int deriving (Show, Eq)
constrain :: [Row] -> Row -> [Row]
constrain = undefined
infer :: [Row] -> Row
infer = undefined
-- TODO: probably this should be wrapped in State instead
readRow :: Axis -> Int -> Puzzle -> Row
readRow = undefined
writeRow :: Axis -> Int -> Row -> Puzzle -> Puzzle
writeRow = undefined
{-
step through each candidate rowset:
  read mtaching row
  skip if it has no unknown cells
  remove candidates that do not match row
  infer new row based on candidates
  write new row [if it has changed from the matching row]
  stop when all rows [all cells] in the puzzle have no unknown cells
  (or when a complete cycle fails to make a changeto the puzzle)
-}
loop = undefined
-- TODO: It's possible to optimize `expand` by passing in an existing row as constraint
-- but consuming a row is a lot harder than consuming an integer
expand :: Pattern -> Int -> [Row]
expand [] 0 = return []
expand [] _ = mzero
expand (Num i:pattern) n | i <= n = map (expansion++) (expand pattern (n - i))
  where expansion = [Black | _ <- [0..n - 1]]
expand (Num i:pattern) n = mzero 
expand (Star:[]) n = return [White | _ <- [0..n - 1]]
expand (Star:pattern) n = return [] -- TODO: Write expansion for Plus and Star
expand (Plus:pattern) 0 = mzero 
expand (Plus:[]) n = return [White | _ <- [0..n - 1]]
expand (Plus:pattern) n = return [White] -- TODO: Write expansion for Plus and Star

test input length expected = do
  let actual = expand input length
  putStr "\n\tinput: "
  print input
  putStr "\tactual: "
  print actual
  putStr "\texpected: "
  print expected
  if actual == expected then putStrLn "success" else putStrLn " !!! FAIL !!!"

main = do
  test [] 0 [[]] 
  test [] 12 []
  test [Num 2] 2 [[Black, Black]]
  test [Num 2, Num 2] 4 [[Black, Black, Black, Black]]
  test [Num 2, Star] 2 [[Black, Black]]
  test [Num 2, Star] 4 [[Black, Black, White, White]]
  test [Star] 2 [[White, White]]
