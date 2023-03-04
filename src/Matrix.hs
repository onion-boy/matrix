module Matrix
  ( new,
    get,
    set,
    determinant,
    add,
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

get :: Matrix -> Position -> Int
get matrix pos = matrix !! fst pos !! snd pos

set :: Matrix -> Position -> Int -> Matrix
set matrix pos value =
  take row matrix
    ++ [ take col mute
           ++ [value]
           ++ drop (col + 1) mute
       ]
    ++ drop (row + 1) matrix
  where
    row = fst pos
    col = snd pos
    mute = matrix !! row

index:: [a] -> [(Int, a)]
index = zip [0 ..]

cofactor :: Matrix -> Position -> Int
cofactor matrix pos =
  if length matrix == 1
    then 1
    else coef * determinant (reduce matrix pos)
  where
    coef = (-1) ^ uncurry (+) pos

reduce :: Matrix -> Position -> Matrix
reduce matrix pos =
  [ [ element | (j, element) <- index column, j /= col ]
    | (i, column) <- index matrix, i /= row
  ]
  where
    row = fst pos
    col = snd pos

determinant :: Matrix -> Int
determinant matrix =
  if length matrix == 1
    then get matrix (0, 0)
    else sum [cofactor matrix (0, i) * get matrix (0, i) | i <- [0 .. (length (head matrix) - 1)]]

add :: Matrix -> Matrix -> Matrix
add first second =
  [[uncurry (+) values | values <- zip column (second !! j)] | (j, column) <- index first]