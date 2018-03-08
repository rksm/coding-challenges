module Main where

import Control.Applicative
import System.IO
import Data.List
import Data.Maybe
import Control.Monad

type Square = [Int]

isMagicSquareList :: Square -> Bool
isMagicSquareList [ a, b, c
                  , d, e, f
                  , g, h, i] = isMagic
  where row1 = [a, b ,c]
        row2 = [d, e, f]
        row3 = [g, h, i]
        col1 = [a, d, g]
        col2 = [b, e, h]
        col3 = [c, f, i]
        diag1 = [a, e, i]
        diag2 = [c, e, g]
        sums = [ sum row1, sum row2, sum row3
               , sum col1, sum col2, sum col3
               , sum diag1, sum diag2]
        magicFold (sum:sums) = foldM (\ a b -> if a == b then Just b else Nothing) sum sums
        isMagic = case magicFold sums of
                    Nothing -> False
                    _ -> True
isMagicSquareList _ = False

includes :: Int -> [Int] -> Bool
includes n = (/= 0) . length . filter (== n)

generateSquaresWithDeltas :: Square -> Square -> [(Square, Square)]
generateSquaresWithDeltas deltas square =
  let i = length deltas
  in
    if i == length square
    then if isMagicSquareList square
         then [(square, deltas)]
         else []
    else do
      let (front, x:back) = splitAt i square
      stepN <- [0..9]
      stepNSigned <- nub [stepN, -stepN]
      let x' = x + stepNSigned
      guard $ x' <= 9 && x' >= 1 && not (includes x' front)
      generateSquaresWithDeltas (deltas ++ [stepNSigned]) (front ++ [x'] ++ back)

magn (_, deltas) = (sum . map abs) deltas

main :: IO ()
main = do
  s_temp <- getMultipleLines 3
  let s = map ( map ( read :: String -> Int ) . words ) s_temp
  let minMagn minSteps soln = min minSteps (magn soln)
      solutions = generateSquaresWithDeltas [] . concat
  print $ foldl minMagn maxBound $ solutions s


getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        back <- getMultipleLines (n-1)
        let ret = (x:back)
        return ret
