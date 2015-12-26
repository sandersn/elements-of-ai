import Control.Monad (mzero)
import Control.Monad.State (get, put, State, runState, evalState, execState)
data Pix = B | W | X deriving (Show, Eq)
pixEq X _ = True -- (should never happen here)
pixEq _ X = True
pixEq B B = True
pixEq W W = True
pixEq _ _ = False

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
a |> f = f a
-- TODO: Probably there's a more concise way to write this.
readRow :: Int -> State Puzzle Row
readRow i = do
  puzzle <- get
  let len = length puzzle
  return (if i < len 
          then puzzle !! i 
          else map (!!i - len) puzzle)
writeRow :: Int -> Row -> State Puzzle ()
writeRow i row = do
  puzzle <- get
  let len = length puzzle
  put (if i < len 
       then replace i row puzzle
       else map (uncurry $ replace (i - len)) (zip row puzzle))
replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i + 1) xs
{-
step through each candidate rowset:
  read matching row
  skip if it has no X cells
  remove candidates that do not match row
  infer new row based on candidates
  write new row [if it has changed from the matching row]
  stop when all rows [all cells] in the puzzle have no X cells
  (or when a complete cycle fails to make a changeto the puzzle)
-}
finished :: Puzzle -> Bool
finished = all $ all (/=X)
finished2 :: Candidateset -> Bool
finished2 = all $ (==1) . length
allRows :: Puzzle -> [Row]
allRows puzzle = rows ++ columns
  where rows = puzzle
        columns = [map (!!i) puzzle | i <- [0..length (head puzzle) - 1]]
solve :: [Pattern] -> Int -> Int -> Puzzle
solve patterns height width = loop (candidates, newPuzzle)
  where newPuzzle = replicate width (replicate height X)
        candidates = generate height width patterns
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
generate :: Int -> Int -> [Pattern] -> Candidateset
generate height width patterns = map generate' (zip [0..] patterns)
  where generate' (i,pattern) = expand pattern (if i < width then width else height)
expand :: Pattern -> Int -> [Row]
expand [] 0 = return []
expand [] _ = mzero
expand (Num i:pattern) n | i <= n = expand' pattern B n i
expand (Num i:pattern) _ = mzero 
expand (Star:[]) n = return $ replicate n W
expand (Star:pattern) n = concat [expand' pattern W n i | i <- [0..n - 1]]
expand (Plus:pattern) 0 = mzero
expand (Plus:pattern) n = concat [expand' pattern W n i | i <- [1..n]]
expand' :: Pattern -> Pix -> Int -> Int -> [Row]
expand' pattern pix n i = 
  map (replicate i pix ++) (expand pattern (n - i))
constrain :: Row -> Rowset -> Rowset
constrain truth = filter rowEq
  where rowEq row = zip row truth |> all (uncurry pixEq)
infer :: Rowset -> Row
infer [] = []
infer rowset | head rowset == [] = []
infer rowset = infer' (map head rowset) : infer (map tail rowset)
  where infer' row | all (==B) row = B
        infer' row | all (==W) row = W
        infer' _ = X

-- test code --
slingshot = [[Star, Num 2, Plus, Num 2, Star], 
             [Star, Num 1, Plus, Num 1, Star], 
             [Star, Num 3, Star], 
             [Star, Num 1, Star], 
             [Star, Num 1, Star], 
             -- columns
             [Star, Num 2, Star], 
             [Star, Num 1, Plus, Num 1, Star], 
             [Star, Num 3, Star], 
             [Star, Num 1, Plus, Num 1, Star], 
             [Star, Num 2, Star]]
miniPuzzle = [[X, B, B],
              [X, X, X],
              [B, W, B]]
main = do
  putStrLn "\t*** Testing `expand` ***"
  testExpand [] 0 [[]]
  testExpand [] 12 []
  testExpand [Num 2] 2 [[B, B]]
  testExpand [Num 2, Num 2] 4 [[B, B, B, B]]
  testExpand [Num 2, Star] 2 [[B, B]]
  testExpand [Num 2, Star] 4 [[B, B, W, W]]
  testExpand [Star] 2 [[W, W]]
  testExpand [Star, Num 2] 4 [[W, W, B, B]]
  testExpand [Star, Num 2, Star] 4 [[B, B, W, W],
                                    [W, B, B, W],
                                    [W, W, B, B]]
  testExpand [Plus] 0 []
  testExpand [Plus] 1 [[W]]
  testExpand [Plus] 2 [[W, W]]
  testExpand [Num 1, Plus, Num 1] 4 [[B, W, W, B]]
  testExpand [Plus, Num 1, Plus] 4 [[W, B, W, W],
                                    [W, W, B, W]]
  testExpand [Star, Num 1, Plus, Num 1, Star]
             4
             [[B, W, B, W],
              [B, W, W, B],
              [W, B, W, B]]
  putStrLn "\t*** Testing `constrain` ***"
  testConstrain [X] [] []
  testConstrain [] [[]] [[]]
  testConstrain [X] [[B], [W]] [[B], [W]]
  testConstrain [B] [[B], [W]] [[B]]
  testConstrain [W] [[B], [W]] [[W]]
  testConstrain [B, W, X] [[B, B, B],
                           [B, W, W],
                           [B, W, B],
                           [W, W, W]]
                          [[B, W, W],
                           [B, W, B]]
  putStrLn "\t*** Testing `infer` ***"
  testInfer [] []
  testInfer [[]] []
  testInfer [[B]] [B]
  testInfer [[W]] [W]
  testInfer [[B], [W]] [X]
  testInfer [[B, W, B], [B, W, W]] [B, W, X]
  putStrLn "\t***Testing `generate` ***"
  testGenerate 5 5 slingshot
                   [[[B,B,W,B,B]],
                    [[B,W,B,W,W],[B,W,W,B,W],[B,W,W,W,B],[W,B,W,B,W],[W,B,W,W,B],[W,W,B,W,B]],
                    [[B,B,B,W,W],[W,B,B,B,W],[W,W,B,B,B]],
                    [[B,W,W,W,W],[W,B,W,W,W],[W,W,B,W,W],[W,W,W,B,W],[W,W,W,W,B]],
                    [[B,W,W,W,W],[W,B,W,W,W],[W,W,B,W,W],[W,W,W,B,W],[W,W,W,W,B]],
                    [[B,B,W,W,W],[W,B,B,W,W],[W,W,B,B,W],[W,W,W,B,B]],
                    [[B,W,B,W,W],[B,W,W,B,W],[B,W,W,W,B],[W,B,W,B,W],[W,B,W,W,B],[W,W,B,W,B]],
                    [[B,B,B,W,W],[W,B,B,B,W],[W,W,B,B,B]],
                    [[B,W,B,W,W],[B,W,W,B,W],[B,W,W,W,B],[W,B,W,B,W],[W,B,W,W,B],[W,W,B,W,B]],
                    [[B,B,W,W,W],[W,B,B,W,W],[W,W,B,B,W],[W,W,W,B,B]]]
  putStrLn "\t***Testing `readRow` ***"
  testReadRow miniPuzzle 0 (miniPuzzle !! 0)
  testReadRow miniPuzzle 2 (miniPuzzle !! 2)
  testReadRow miniPuzzle 3 (map (!!0) miniPuzzle)
  testReadRow miniPuzzle 5 (map (!!2) miniPuzzle)
  putStrLn "\t***Testing `writeRow` ***"
  testWriteRow miniPuzzle 2 [W, W, W] [[X, B, B],
                                       [X, X, X],
                                       [W, W, W]]
  testWriteRow miniPuzzle 3 [W, W, W] [[W, B, B],
                                       [W, X, X],
                                       [W, W, B]]

testInfer rowset expected = do
  let actual = infer rowset
  if actual == expected
  then putStr "." -- putStrLn ("success for " ++ show actual)
  else do
    putStrLn " !!! FAIL !!!"
    putStr "\trowset: "
    print rowset
    putStr "\tactual: "
    print actual
    putStr "\texpected: "
    print expected
testConstrain truth rowset expected = do
  let actual = constrain truth rowset
  if actual == expected
  then putStr "." -- putStrLn ("success for " ++ show truth)
  else do
    putStrLn " !!! FAIL !!!"
    putStr "\ttruth: "
    print truth
    putStr "\tactual: "
    print actual
    putStr "\texpected: "
    print expected
testExpand input length expected = do
  let actual = expand input length
  if actual == expected
  then putStr "." -- putStrLn ("success for " ++ show input)
  else do
    putStrLn " !!! FAIL !!!"
    putStr "\tinput: "
    print input
    putStr "\tactual: "
    print actual
    putStr "\texpected: "
    print expected
testGenerate height width patterns expected = do
  let actual = generate height width patterns
  if actual == expected
  then putStr "." -- putStrLn ("success for " ++ show patterns)
  else do
    putStrLn " !!! FAIL !!!"
    putStr "\tpatterns: "
    print patterns
    putStr "\tactual: "
    print actual
    putStr "\texpected: "
    print expected
testReadRow puzzle i expected = do
  let (actualRow, actualPuzzle) = runState (readRow i) puzzle
  if actualPuzzle /= puzzle
  then fail "readRow should not modify its puzzle"
  else if actualRow == expected
       then putStr "." -- putStrLn ("success for " ++ show i)
       else do
         putStrLn " !!! FAIL !!!"
         putStr "\ti: "
         print i
         putStr "\tactual row: "
         print actualRow
         putStr "\texpected row: "
         print expected
testWriteRow puzzle i row expected = do
  let actualPuzzle = execState (writeRow i row) puzzle
  if actualPuzzle == expected
  then putStr "." -- putStrLn ("success for " ++ show row)
  else do
    putStrLn " !!! FAIL !!!"
    putStr "\tactual puzzle: "
    print actualPuzzle
    putStr "\texpected: "
    print expected