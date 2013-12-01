import Text.Printf (printf)
import Debug.Trace (trace, traceShow)
import Data.Array
import Data.List (partition)
import Criterion.Main
import Criterion.Config
import System.Random

{- 
minfree xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (not . (`elem` vs)) us
--}


accumArray2 f init bnds vals = 
    let arr = array bnds [(i, init) | i <- range bnds]
        acc a (i, v) = a // [(i, f (a ! i) v)]
    in foldl acc arr vals


-- debug helper
debug a b = if debugOn
            then trace b a
            else a
    where debugOn = False


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
minfrom a xs = helper a xs `debug` (show (us, vs, b))
   where b = a + 1 + (length xs) `div` 2
         (us, vs) = partition (< b) xs
         helper a xs
          | null xs            = a
          | length us == b - a = minfrom b vs `debug`
                                     (printf "minfrom %d %s" b $ show vs)

          | otherwise          = minfrom a us `debug`
                                     (printf "minfrom %d %s" a $ show us)

-- one thing to do is to profile the performance various ways to choose b
-- as far as I can see, Bird hasn't justified the use of b = a + 1 + n div 2
-- in any way. Ideally, if we had access to the median of the input, we would
-- use the median for b. However, it is too expensive to compute the median.
-- So the question is: in what way is his choice a good approximation to the
-- median?
--
-- v. interesting: b = a + n div 2 fails if we're given a = 1, xs = [1]. in
-- this case, b = 1, us = [], vs = [1], so infinite loop
--
-- so that's one reason why b = a + 1 + n div 2. it forces b to always be different
-- from a, so that we cant infinite loop by taking the middle route (in the case
-- where length us == b - a, since that would require b == a)
--
-- if n > b, us can only contain (b - a) elements at most, and a >= 0, so yes,
-- us must be a proper sublist of xs if n > b. ditto for n == b.
--
-- TODO: prove that the choice of b makes infinite loop impossible


-- testing

getRandList :: RandomGen g => g -> (Int, Int) -> Int -> [Int]
getRandList gen r 0 = []
getRandList gen r n = v:rest where
    (v, gen') = randomR r gen
    rest = getRandList gen' r (n - 1)

main :: IO ()
main = do
    gen <- getStdGen
    inDataA <- return $ getRandList gen (0, 300) 80
    inDataB <- return $ getRandList gen (0, 300) 120

    defaultMainWith
      defaultConfig { cfgSamples = ljust 1000 }
      (return ())
      [ bench "minfree 80"  $ nf minfree inDataA
      , bench "minfree 120" $ nf minfree inDataB
      ]

