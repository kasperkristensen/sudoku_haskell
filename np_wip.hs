import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

data Field = Settled Int | Possible [Int] deriving (Show, Eq) 
type Row = [Field] 
type Sudoku = [Row]

testSudoku :: String 
testSudoku = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"

chunks :: Int -> [a] -> [[a]] 
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

loadField :: Char -> Maybe Field
loadField '0' = Just $ Possible [1..9] 
loadField f
    | isDigit f && f > '0' = Just (Settled (digitToInt f))
    | otherwise = Nothing

loadSudoku :: String -> Maybe Sudoku 
loadSudoku s
    | length s == 81 = traverse (traverse loadField) . chunks 9 $ s
    | otherwise = Nothing
    
showField :: Field -> String 
showField (Settled f) = show f
showField _ = "0" 

showSudoku :: Sudoku -> String
showSudoku = unlines . map (unwords . map showField)

findSettled :: [Field] -> [Int]
findSettled xs = [x | Settled x <- xs]

countPossibles :: Field -> Int 
countPossibles (Possible fs) = length fs 
countPossibles (Settled _) = 1

isPossible :: Field -> Bool 
isPossible (Possible _) = True
isPossible _ = False

removeSettledFromPos :: [Field] -> Maybe [Field] 
removeSettledFromPos fields = traverse remove fields
    where
        remove (Possible xs) = case xs \\ (findSettled fields) of
            [] -> Nothing 
            [f] -> Just $ Settled f 
            fs -> Just $ Possible fs 
        remove e = Just e

subGridsToRows :: Sudoku -> Sudoku 
subGridsToRows =
  concatMap (\rows -> let [row1, row2, row3] = map (chunks 3) rows
    in zipWith3 (\a b c -> a ++ b ++ c) row1 row2 row3) . chunks 3

removeSettledFromAllPos :: Sudoku -> Maybe Sudoku 
removeSettledFromAllPos s = do
    x1 <- traverse removeSettledFromPos s 
    x2 <- fmap transpose . traverse removeSettledFromPos . transpose $ x1 
    x3 <- fmap subGridsToRows . traverse removeSettledFromPos . subGridsToRows $ x2
    return x3

removeSettledFromAllPos' :: Sudoku -> Maybe Sudoku
removeSettledFromAllPos' = fixPoint removeSettledFromAllPos
  where
    fixPoint f x = f x >>= \x' -> if x' == x then return x else fixPoint f x'

findMostRestricted :: Sudoku -> (Int, Field) 
findMostRestricted s = minimumBy (compare `on` countPossibles . snd) (filter (isPossible . snd) (zip [0..] (concat s))) -- it returns a double containing the position of the Field (from 0 - 80), aswell as the Fields value.

fixField :: (Int, Field) -> (Int, Field, Field)
fixField (i, Possible [x, y]) = (i, Settled x, Settled y) 
fixField (i, Possible (x:xs)) = (i, Settled x, Possible xs) 
fixField _ = error "Invalid input"

makeBranch :: Int -> a -> [[a]] -> [[a]] 
makeBranch i v =
    let (x, y) = (i `quot` 9, i `mod` 9) in replace x (replace y (const v)) 
    where
        replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]] 

branch :: Sudoku -> (Sudoku, Sudoku)
branch s = 
    let (index, settled, rest) = fixField . findMostRestricted $ s
    in (makeBranch index settled s, makeBranch index rest s)

isSudokuFilledIn :: Sudoku -> Bool
isSudokuFilledIn s = null [() | Possible _ <- concat s]

checkDuplicates :: [Int] -> Bool
checkDuplicates r = checkDuplicates' r []
    where
        checkDuplicates' [] _ = False
        checkDuplicates' (x:xs) ys
            | x `elem` ys = True
            | otherwise = checkDuplicates' xs (x:ys)

checkRow :: Row -> Bool
checkRow row = 
    let settledFields = [field | Settled field <- row]
        emptyPossibles = [field | Possible field <- row, null field]
    in checkDuplicates settledFields || not (null emptyPossibles)

checkSolution :: Sudoku -> Bool 
checkSolution s = any checkRow s || any checkRow (transpose s) || any checkRow (subGridsToRows s)

solve :: Sudoku -> Maybe Sudoku
solve s = do 
    a <- removeSettledFromAllPos' s
    solve' a
  where
    solve' s'
      | checkSolution s' = Nothing
      | isSudokuFilledIn s'  = Just s'
      | otherwise =
          let (branch1, branch2) = branch s'
          in solve branch1 <|> solve branch2

parsePuzzles :: [String] -> [Sudoku]
parsePuzzles [] = []
parsePuzzles lines =
    let (first, rest) = splitAt 10 lines
        sudoku = fromJust (loadSudoku (concat (tail first)))
    in sudoku : parsePuzzles rest

main = do
    input <- readFile "sudoku.txt"
    let puzzles = parsePuzzles (lines input)
        solved = map solve puzzles
        toSolution Nothing = "no solution\n"
        toSolution (Just sudoku) = "-----\n" ++ showSudoku sudoku
    putStr (concatMap toSolution solved)