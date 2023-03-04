module Main (main) where

import Lib

main :: IO ()
main =
  let fmatrix = [[0, 2, 5, 6], [-1, 3, 4, 8], [9, 3, 4, -2], [3, 5, -7, 2]]
      smatrix = [[0, 4, 3, 8], [2, 7, 3, 4], [-3, 5, 6, -1], [5, 3, -2, 7]]
      prod = smatrix `multiply` fmatrix
  in print prod