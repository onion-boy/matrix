module Matrix
  ( new,
    valid,
    get,
    set,
    determinant,
    add,
    multiply,
    transpose,
    Matrix,
  )
where

type Matrix = [[Int]]

type Position = (Int, Int)

-- Create a new matrix with size (rows, cols)
new :: Position -> Matrix
new size = replicate rows (replicate cols 0)
  where
    rows = fst size
    cols = snd size

-- Check if a matrix is rectangular; all columns are the same size
valid :: Matrix -> Bool
valid [] = False
valid [_] = True
valid (first : remains) =
  length first == length (head remains) && valid remains

-- Check if a matrix is square; # rows == # cols
square :: Matrix -> Bool
square matrix =
  length matrix == length (head matrix)

-- Check if the given position is indexable; does Matrix[i][j] exist?
within :: Position -> Matrix -> Bool
pos `within` matrix =
  row < length matrix && col < length (head matrix) && all (>= 0) pos
  where
    row = fst pos
    col = snd pos

-- Check if two matrices have the same dimensions
congruent :: Matrix -> Matrix -> Bool
first `congruent` second =
  first_rows == second_rows && first_cols == second_cols
  where
    first_rows = length first
    first_cols = length (head first)
    second_rows = length second
    second_cols = length (head second)

-- Check if two matrices have compatible dimensions; can they be multiplied?
compatible :: Matrix -> Matrix -> Bool
first `compatible` second =
  first_rows == second_cols
  where
    first_rows = length first
    second_cols = length (head second)

-- Get one element of a matrix at the given position
get :: Matrix -> Position -> Maybe Int
matrix `get` pos
  | pos `within` matrix = Just value
  | otherwise = Nothing
  where
    value = matrix !! fst pos !! snd pos

-- Set one element of a matrix at the given position
set :: Matrix -> Position -> Int -> Maybe Matrix
set matrix pos value
  | pos `within` matrix = Just updated
  | otherwise = Nothing
  where
    row = fst pos
    col = snd pos
    mute = matrix !! row
    updated =
      take row matrix
        ++ [ take col mute
               ++ [value]
               ++ drop (col + 1) mute
           ]
        ++ drop (row + 1) matrix

-- Zip one array into (index, element)
index :: [a] -> [(Int, a)]
index = zip [0 ..]

-- Get a subset of the given matrix that includes all points except the ones in the same row/column as the given position
reduce :: Matrix -> Position -> Maybe Matrix
matrix `reduce` pos
  | square matrix =
      Just
        [ [element | (j, element) <- index column, j /= col]
          | (i, column) <- index matrix,
            i /= row
        ]
  | otherwise = Nothing
  where
    row = fst pos
    col = snd pos

-- Find the reduced matrix of the position and either add or subtract it, based on the position
cofactor :: Matrix -> Position -> Maybe Int
cofactor matrix pos
  | square matrix && pos `within` matrix = Just (* coef) <*> (determinant =<< matrix `reduce` pos)
  | otherwise = Nothing
  where
    coef = (-1) ^ uncurry (+) pos

-- Find the determinant of the given matrix
determinant :: Matrix -> Maybe Int
determinant matrix
  | square matrix && length matrix == 1 = matrix `get` (0, 0)
  | square matrix =
      sum
        <$> sequence
          [ Just (*) <*> cofactor matrix (0, i) <*> matrix `get` (0, i) | i <- [0 .. (length (head matrix) - 1)]
          ]
  | otherwise = Nothing

-- Add two matrices
add :: Matrix -> Matrix -> Maybe Matrix
first `add` second
  | first `congruent` second = Just [[uncurry (+) values | values <- zip column (second !! j)] | (j, column) <- index first]
  | otherwise = Nothing

-- Multiply two matrices using very sketchy recursion
multiplyRecursive :: Matrix -> Matrix -> Matrix
[] `multiplyRecursive` [] = []
[] `multiplyRecursive` (_ : _) = []
(f : fs) `multiplyRecursive` second =
  [sum (zipWith (*) f col) | col <- transpose second] : fs `multiplyRecursive` second

-- Multiply two matrices
multiply :: Matrix -> Matrix -> Maybe Matrix
first `multiply` second
  | first `compatible` second = Just (first `multiplyRecursive` second)
  | otherwise = Nothing

-- Transpose the given matrix
transpose :: Matrix -> Matrix
transpose ([] : _) = []
transpose matrix =
  map head matrix : transpose (map tail matrix)