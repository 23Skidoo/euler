{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}

module Common (inputFileInteract, inputLinesInteract,
    readWords, numberToDigits, defactorize,
    digitsToNumber, digits, digits10, digits2, isPalindromic, fac, binomial,
    uniq, permutations, tabulate, dp, isTriangle, triangle, isPentagonal,
    pentagonal, isHexagonal, hexagonal, isPrime, primes, nthPrime, factorize,
    divisors, num_divisors, proper_divisors, num_proper_divisors)
    where

import Data.Array (Ix, Array, (!), array, range)
import Data.Char (digitToInt)
import Data.List ((\\), delete, group)
import qualified Math.Sieve.Factor as F
import System.Environment (getArgs)
import System.IO (Handle, IOMode(ReadMode), openFile, hGetLine, hIsEOF, stdin)

-- A version of 'interact' that can read files provided on command-line.
-- Used like this: main = inputFileInteract processInput.
inputFileInteract :: (Show b) => (String -> b) -> IO ()
inputFileInteract processInput =
    do args <- getArgs
       case args of
         [] ->  getContents >>= (print . processInput)
         (x:_) -> readFile x >>= (print . processInput)

-- Like 'inputFileInteract', but processes input line-by-line.
inputLinesInteract :: forall state .
                      (state -> String -> state) -> state -> IO state
inputLinesInteract processLine initialState = do
  args <- getArgs
  case args of
    []    -> go initialState stdin
    (x:_) -> openFile x ReadMode >>= go initialState

  where
    go :: state -> Handle -> IO state
    go !s h = do
      eof <- hIsEOF h
      if eof
        then return s
        else do l <- hGetLine h
                go (processLine s l) h


-- "\"abc\", \"def\", \"ghi\"" -> ["abc", "def", "ghi"].
readWords :: String -> [String]
readWords st = readNames' False "" st
    where
      readNames' _ _ []           = []
      readNames' False s ('"':ss) = readNames' True s ss
      readNames' False s (_:ss)   = readNames' False s ss
      readNames' True s ('"':ss)  = s : readNames' False "" ss
      readNames' True s (c:ss)    = readNames' True (s ++ [c]) ss

-- 123 <-> [1,2,3].

numberToDigits :: (Show a, Integral a) => a -> [Int]
numberToDigits = map digitToInt . show

digitsToNumber :: (Enum a, Num a) => [Int] -> a
digitsToNumber = fst . makeNumber'
    where
      makeNumber' [] = (0, 0)
      makeNumber' (x:xs) = let (y, k) = makeNumber' xs
                           in (toEnum x*10^k + y, k+1)

-- Number of digits in base-b representation.
-- We can actually use an integer logarithm here.
digits :: (Num t1, Integral t) => t -> t -> t1
digits n b = digits' n 0
    where
      digits' 0 k = k
      digits' m k = digits' (m `div` b) (k+1)

digits10 :: (Integral t, Num t1) => t -> t1
digits10 n = digits n 10

digits2 :: (Integral t, Num t1) => t -> t1
digits2 n = digits n 2

-- Is this number palindromic?

isPalindromic :: (Show a, Integral a) => a -> Bool
isPalindromic n = reverse ndigits == ndigits
    where
      ndigits = numberToDigits n

-- Factorial and binomial coefficients
fac :: (Num t, Enum t) => t -> t
fac n = product [1..n]

binomial :: (Integral a) => a -> a -> a
binomial n k = product [n-k+1..n] `div` fac k

-- STL-like uniq - basically, a fast nub for sorted lists.
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x:y:ys) = if x == y
                then uniq (y:ys)
                else x : uniq (y:ys)

-- All n! permutations of a list.
permutations :: (Eq a) => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ x : ps | x <- xs , ps <- permutations ( xs \\ [x]) ]

-- Memoization.

tabulate :: (Ix i) => (i, i) -> (i -> e) -> Array i e
tabulate bounds f = array bounds [(i,f i) | i <- range bounds]

dp :: (Ix i) => (i, i) -> ((i -> e) -> i -> e) -> i -> e
dp bounds f = (memo!)  where memo = tabulate bounds (f (memo!))

-- Triangle, pentagonal and hexagonal numbers.
triangle :: (Num a, Enum a, Integral a) => [a]
triangle = [n*(n + 1) `div` 2 | n <- [1..]]

isTriangle :: (Integral a) => a -> Bool
isTriangle n = (fromIntegral . ceiling $ test) == test
    where
      test :: Double
      test = (sqrt(8 * fromIntegral n + 1) - 1)/2

pentagonal :: (Num a, Enum a, Integral a) => [a]
pentagonal = [n*(3*n - 1) `div` 2 | n <- [1..]]

isPentagonal :: (Integral a) => a -> Bool
isPentagonal n = (fromIntegral . ceiling $ test) == test
    where
      test :: Double
      test = (sqrt (24 * fromIntegral n + 1) + 1)/6

hexagonal :: (Num a, Enum a, Integral a) => [a]
hexagonal = [n*(2*n - 1) | n <- [1..]]

isHexagonal :: (Integral a) => a -> Bool
isHexagonal n = (fromIntegral . ceiling $ test) == test
    where
      test :: Double
      test = (sqrt (8 * fromIntegral n + 1) + 1)/4

-- Primality test and prime number generator.

isPrime :: (Integral a) => a -> Bool
isPrime = F.isPrime factorSieve

factorSieve :: F.FactorSieve
factorSieve = F.sieve (1000000 :: Int)

primes :: (Integral a) => [a]
primes = filter isPrime [2..]

nthPrime :: (Integral a) => Int -> a
nthPrime n = primes !! (n-1)

-- Prime number factorization.
factorize :: (Integral a) => a -> [(a, Int)]
factorize x
    = let divides m n = n `mod` m == 0

          prime_factors 1 = []
          prime_factors n = factor : prime_factors (n `div` factor)
              where
                factor = head $ filter (`divides` n) primes

      in map (\l -> (head l, length l)) $ group $ prime_factors x

-- Inverse of factorize.
defactorize :: (Integral a, Integral b) => [(a, b)] -> a
defactorize = product . map (\(x,y) -> x^y)

--prop_factorize :: Integral a => a -> Bool
--prop_factorize n = ( defactorize . factorize $ n) == n

-- How many integer divisors does a number have?
num_divisors :: (Integral a) => a -> Int
num_divisors = product . map (\(_,y) -> y+1) . factorize

-- Return all divisors of this number.
divisors :: (Integral a) => a -> [a]
divisors k = divisors' . factorize $ k
    where
      divisors' [] = []
      divisors' [(x,n)] = [(x^nn) | nn <- [0..n]]
      divisors' ((x,n):xs) = [(x^nn)*y | nn <- [0..n], y <- divisors' xs ]

-- Return all proper divisors of this number.
proper_divisors :: (Integral a) => a -> [a]
proper_divisors n = delete n . divisors $ n

-- How many proper divisors does a number have?
num_proper_divisors :: (Integral a) => a -> Int
num_proper_divisors = subtract 1 . num_divisors
