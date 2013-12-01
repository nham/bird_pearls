import Text.Printf (printf)
import Debug.Trace (trace, traceShow)
import Data.Array
import Data.List (partition)

{- 
minfree xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (not . (`elem` vs)) us
--}


accumArray2 f init bnds vals = 
    let arr = array bnds [(i, init) | i <- range bnds]
        acc a (i, v) = a // [(i, f (a ! i) v)]
    in foldl acc arr vals


-- Array-based implementation of minfree
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist is =
    accumArray (\x y -> True) False bnds [(i, i) | i <- is, inRange bnds i]
            where bnds = (0, length is)

-- divide and conquer
minfree xs = minfrom 0 xs
-- assuming inputs are all greater than or equal to a
-- also, the list must all be distinct
minfrom a xs = traceShow (us, vs, b) $ helper a xs
   where b = a +  (length xs) `div` 2
         (us, vs) = partition (< b) xs
         helper a xs
          | null xs            = a
          | length us == b - a = trace (printf "minfrom %d %s" b $ show vs)
                                       minfrom b vs
          | otherwise          = trace (printf "minfrom %d %s" a $ show us)
                                       minfrom a us
