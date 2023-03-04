module Matrix
  ( new,
    get,
    set,
    determinant,
    add,
    multiply,
    Matrix,
  )
where

type Matrix = [[Int]]

type Position = (Int, Int)

new :: Position -> Matrix
new size = replicate rows (replicate cols 0)
  where
    rows = fst size
    cols = snd size

square :: Matrix -> Bool
square matrix =
  valid matrix && length matrix == length (head matrix)

valid :: Matrix -> Bool
valid [] = False
valid [_] = True
valid (first : remains) =
  length first == length (head remains) && valid remains

within :: Position -> Matrix -> Bool
pos `within` matrix =
  valid matrix && row < length matrix && col < length (head matrix) && all (>= 0) pos
  where
    row = fst pos
    col = snd pos

congruent :: Matrix -> Matrix -> Bool
first `congruent` second =
  valid first && valid second && first_rows == second_rows && first_cols == second_cols
  where
    first_rows = length first
    first_cols = length (head first)
    second_rows = length second
    second_cols = length (head second)

compatible :: Matrix -> Matrix -> Bool
first `compatible` second =
  valid first && valid second && first_rows == second_cols
  where
    first_rows = length first
    second_cols = length (head second)

get :: Matrix -> Position -> Maybe Int
matrix `get` pos
  | pos `within` matrix = Just value
  | otherwise = Nothing
  where
    value = matrix !! fst pos !! snd pos

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

index :: [a] -> [(Int, a)]
index = zip [0 ..]

cofactor :: Matrix -> Position -> Maybe Int
cofactor matrix pos
  | square matrix && length matrix == 1 = Just 1
  | square matrix && pos `within` matrix = Just (* coef) <*> (determinant =<< matrix `reduce` pos)
  | otherwise = Nothing
  where
    coef = (-1) ^ uncurry (+) pos

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

determinant :: Matrix -> Maybe Int
determinant matrix
  | square matrix && length matrix == 1 = matrix `get` (0, 0)
  | square matrix =
      sum
        <$> sequence
          [ Just (*) <*> cofactor matrix (0, i) <*> matrix `get` (0, i) | i <- [0 .. (length (head matrix) - 1)]
          ]
  | otherwise = Nothing

add :: Matrix -> Matrix -> Maybe Matrix
first `add` second
  | first `congruent` second = Just [[uncurry (+) values | values <- zip column (second !! j)] | (j, column) <- index first]
  | otherwise = Nothing

multiply :: Matrix -> Matrix -> Maybe Matrix
first `multiply` second
  | first `compatible` second =
      sequence $
        [ sequence $
            [ sum
                <$> sequence
                  [ Just (*) <*> first `get` (i, t) <*> second `get` (t, j) | t <- [0 .. first_cols]
                  ]
              | j <- [0 .. second_cols]
            ]
          | i <- [0 .. first_rows]
        ]
  | otherwise = Nothing
  where
    first_rows = length first - 1
    first_cols = length (head first) - 1
    second_cols = length (head second) - 1
