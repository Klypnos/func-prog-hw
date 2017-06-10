-- /*************************
-- **************************
-- ***** SABRİ ÖZGÜR ********
-- ****** 150130004 *********
-- ****** HOMEWORK 1 ********
-- **************************
-- *************************/

import Data.List
import System.Environment
import Debug.Trace

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readNumbers :: String -> [Int]
readNumbers = map read . words

data Candidate = Candidate { fullName :: String , voteCount :: Int } --deriving Show
class ToString a where --defined the type of toString
    toString :: a -> String

instance ToString Candidate where
    toString c = "\nCandidate "
                  ++shows (fullName c) " "
                  ++show (voteCount c)

instance Show Candidate where
  show = toString -- told show which function to use for Candidate class

generateCandidate :: (String, Int) -> Candidate
generateCandidate x = Candidate (fst x) (snd x)

allSame :: [Int] -> Bool
allSame [] = True
allSame [x] = True
allSame (x:xs)
  | x == head xs = allSame xs
  | otherwise = False

compareTuple :: (Int,Int) -> (Int,Int) -> Ordering --minimum and maximum does not work on tuples by default as expected
compareTuple (i1,a1) (i2,a2) -- this function tells minimumBy function that it sorts according to the second element of the tuple
  | a1 > a2 = GT
  | a1 < a2 = LT
  | otherwise = EQ

calculateResults :: [[Int]] ->[String] -> [Int] -> [Candidate] -- (votes, candidates, filter) -> winners
calculateResults xss ys zs --
  | null xss = []
  | length (filter (\x -> x >= req ) countRes) == 0 && allSame (filter (\x -> x /= 0 ) countRes) == False = calculateResults xss ys elim -- if all non-zero elements are not same
  | otherwise = map generateCandidate (filter (\(a,b) -> b == maximum countRes ) (zip ys countRes))
    where
      elim = map (\(x,y) -> x) (filter (\(x,y) -> y <= snd (minimumBy compareTuple countRes') ) countResIndexed)
      req  = ((length xss) `div` 2) + 1
      countRes = countVotes xss zs
      countResIndexed = zip [1 .. length ys] countRes
      countRes' = filter (\(x,y) -> (x `elem` zs) == False ) countResIndexed

-- votes elim
updateVariable :: [Int] -> Int -> Int -> [Int] -- update an element of a list with given index and return it
updateVariable xs ith val = updateVariable' xs ith val 0
  where
    updateVariable' :: [Int] -> Int -> Int -> Int -> [Int]
    updateVariable' xs ith val curr
      | ith > length xs = error "Out of valid index range"
      | curr == ith = (take ith xs) ++ (val : (drop (ith+1) xs))
      | otherwise = updateVariable' xs ith val (curr+1)

countVotes :: [[Int]] -> [Int] -> [Int] -- counts votes of each candidate looking at the matrix and eliminated candidates list
countVotes xss filt = countVotes' xss (take (length (head xss)) (repeat 0) ) -- matrix and list of zeroes for starting
  where
    countVotes' :: [[Int]] -> [Int] -> [Int]
    countVotes' xss ys
      | null xss = ys
      | null filt = countVotes' (tail xss) (updateVariable ys ind ((ys !! ind)+1))
      | otherwise = countVotes' (tail xss) (updateVariable ys ind2 ((ys !! ind2)+1))
        where
          ind = (head (head xss)) - 1 -- if there is no filter use this index
          findValid :: [Int] -> [Int] -> Int
          findValid (x:xs) ys -- [1,2,3] filt
            | x `elem` ys = findValid xs ys
            | otherwise = x
          ind2 = (findValid (head xss) filt) - 1 -- if theres a filter list, return a valid vote according to the filter

main = do
  [f] <- getArgs
  contents <- readLines f
  --putStrLn (show (length (tail contents)))
  let numofCandidates = read (head contents) :: Int
  --putStrLn (show numofCandidates)
  let fcandidates = take numofCandidates (tail contents)
  --putStrLn (show (length (tail contents)))
  --putStrLn (show fcandidates)
  let fvotes = drop numofCandidates (tail contents)
  --putStrLn (show fvotes)
  let votes = map readNumbers fvotes
  --putStrLn (show votes)
  --mapM_ print votes
  let results = calculateResults votes fcandidates []
  putStrLn "Winner Candidate(s): "
  putStrLn (show results)
