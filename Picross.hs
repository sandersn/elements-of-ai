import Control.Monad (mzero)
data Pix = Black | White | Unknown deriving (Show, Eq)
data Axis = Horizontal | Vertical
type Pattern = [Specifier]
type Row = [Pix]
-- Puzzle is stored as a list of rows.
--  Updating a row is easy - replace the row.
--  Updating a column means replacing a cell in every row.
--  There is certainly a better representation out there.
type Puzzle = [Row]
type Rowset = [Row] -- should be fine
-- stored as [RowCandidates] ++ [ColCandidates]
-- maybe ([RowCandidates], [ColCandidates]) would be better
type Candidateset = [Rowset]
data Specifier = Star | Plus | Num Int deriving (Show, Eq)
constrain :: [Row] -> Row -> [Row]
constrain = undefined
infer :: [Row] -> Row
infer = undefined
-- TODO: probably this should be wrapped in State instead
readRow :: Int -> Puzzle -> Row
readRow i puzzle = if i < len then puzzle !! i else map (!!i - len) puzzle
  where len = length puzzle
writeRow :: Int -> Row -> Puzzle -> Puzzle
writeRow = undefined
{-
step through each candidate rowset:
  read matching row
  skip if it has no unknown cells
  remove candidates that do not match row
  infer new row based on candidates
  write new row [if it has changed from the matching row]
  stop when all rows [all cells] in the puzzle have no unknown cells
  (or when a complete cycle fails to make a changeto the puzzle)
-}
finished :: Puzzle -> Bool
finished = all $ all (/=Unknown)
allRows :: Puzzle -> [Row]
allRows puzzle = rows ++ columns
  where rows = puzzle
        columns = [map (!!i) puzzle | i <- [0..length (head puzzle) - 1]]
solve :: [Pattern] -> Int -> Int -> Puzzle
solve patterns height width = loop (candidates, newPuzzle)
  where newPuzzle = [replicate height Unknown | i <- [0..width -1]]
        candidates = map (generate height width) (zip [0..] patterns)
loop :: (Candidateset, Puzzle) -> Puzzle
loop (candidates, puzzle) =
  if finished puzzle
  then puzzle
  -- ugh. I have to remember how to use State here
  -- then turn map into mapM
  else loop (map (loopBody puzzle) (zip candidates [0..]), puzzle)
-- disabled until I can either wrap in State or write it myself
-- alternatively, I could infer the new puzzle from the returned rowsets
-- but I think it's more correct to update the puzzle incrementally
-- (I'm not sure how to combine row vs column inferences in a batched way)
loopBody :: Puzzle -> (Rowset, Int) -> Rowset -- (Rowset, Puzzle)
loopBody puzzle (rowset, i) = undefined
--loopBody puzzle (rowset, i) = (newSet, writeRow i newRow puzzle)
--  where newSet = constrain rowset (readRow i puzzle)
--        newRow = infer newSet
-- TODO: It's possible to optimize `expand` by passing in an existing row as constraint
-- but consuming a row is a lot harder than consuming an integer
generate :: Int -> Int -> (Int, Pattern) -> [Row]
generate height width (i, pattern) =
  expand pattern (if i < height then height else width)
expand :: Pattern -> Int -> [Row]
expand [] 0 = return []
expand [] _ = mzero
expand (Num i:pattern) n | i <= n = map (replicate i Black ++) (expand pattern (n - i))
  where expansion = replicate i Black
expand (Num i:pattern) _ = mzero 
expand (Star:[]) n = return $ replicate n White
expand (Star:pattern) n = concat [map (replicate i White ++) (expand pattern (n - i)) | i <- [0..n - 1]]
expand (Plus:pattern) 0 = mzero
expand (Plus:pattern) n = concat [map (replicate i White ++) (expand pattern (n - i)) | i <- [1..n]]
main = do
  putStrLn "\t*** Testing `expand` ***"
  testExpand [] 0 [[]]
  testExpand [] 12 []
  testExpand [Num 2] 2 [[Black, Black]]
  testExpand [Num 2, Num 2] 4 [[Black, Black, Black, Black]]
  testExpand [Num 2, Star] 2 [[Black, Black]]
  testExpand [Num 2, Star] 4 [[Black, Black, White, White]]
  testExpand [Star] 2 [[White, White]]
  testExpand [Star, Num 2] 4 [[White, White, Black, Black]]
  testExpand [Star, Num 2, Star] 4 [[Black, Black, White, White],
                                    [White, Black, Black, White],
                                    [White, White, Black, Black]]
  testExpand [Plus] 0 []
  testExpand [Plus] 1 [[White]]
  testExpand [Plus] 2 [[White, White]]
  testExpand [Num 1, Plus, Num 1] 4 [[Black, White, White, Black]]
  testExpand [Plus, Num 1, Plus] 4 [[White, Black, White, White],
                                    [White, White, Black, White]]
  testExpand [Star, Num 1, Plus, Num 1, Star]
             4
             [[Black, White, Black, White],
              [Black, White, White, Black],
              [White, Black, White, Black]]

testExpand input length expected = do
  let actual = expand input length
  if actual == expected
  then putStrLn ("success for " ++ show input)
  else do
    putStrLn " !!! FAIL !!!"
    putStr "\tinput: "
    print input
    putStr "\tactual: "
    print actual
    putStr "\texpected: "
    print expected

