{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.List
import Data.Aeson
import Control.Applicative
import Debug.Trace
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import System.Environment
import Text.Printf

data ScoreSet = ScoreSet { startingPosition :: Int
                         , scores :: [Int]
                         } deriving (Generic, Show)

instance ToJSON ScoreSet
instance FromJSON ScoreSet

data ScoreSheet = ScoreSheet { leftSheet :: [ScoreSet]
                             , rightSheet :: [ScoreSet]
                             } deriving (Generic, Show)

instance ToJSON ScoreSheet
instance FromJSON ScoreSheet

data Game = Game { serves :: Char
                     , leftPos :: Int
                     , leftScore :: Int
                     , rightScore :: Int
                     , rightPos :: Int
                     } deriving (Show)

data Stats = Stats { sideouts :: Int
                   , points :: Int
                   , efficiency :: Float
                   }

instance Show Stats where
  show Stats {sideouts=sdts, points=pts, efficiency=eff} = "sideouts " ++ show sdts ++ " - points " ++ show pts ++ " - efficiency " ++ show eff

-------- For test ---------------------
left :: [ScoreSet]
left = [ ScoreSet {startingPosition=1, scores=[-1, 1, 2, 4, 5, 6, 8, 9, 11, 13, 20, 21, 23]}
        , ScoreSet {startingPosition=6, scores=[2, 3, 5, 8, 12, 15, 17, 18, 19, 22, 25]}
        , ScoreSet {startingPosition=1, scores=[-1, 1, 2, 3, 4, 5, 6, 9, 12, 13, 14, 16, 17, 19, 20]}
        , ScoreSet {startingPosition=4, scores=[0, 1, 2, 3, 5, 6, 7, 8, 9, 16, 17, 20, 22]}
        ]

right :: [ScoreSet]
right = [ ScoreSet {startingPosition=1, scores=[1, 2, 3, 4, 5, 7, 9, 10, 12, 15, 17, 23, 25]}
        , ScoreSet {startingPosition=2, scores=[-1, 2, 3, 4, 10, 14, 15, 16, 17, 18, 20]}
        , ScoreSet {startingPosition=4, scores=[4, 5, 6, 7, 8, 9, 10, 13, 17, 18, 20, 21, 23, 24, 25]}
        , ScoreSet {startingPosition=5, scores=[-1, 4, 5, 7, 8, 11, 12, 13, 15, 18, 19, 22, 23, 25]}
        ]
ss = ScoreSheet { leftSheet=[ ScoreSet {startingPosition=1, scores=[-1, 1, 2, 4, 5, 6, 8, 9, 11, 13, 20, 21, 23]}, ScoreSet {startingPosition=6, scores=[2, 3, 5, 8, 12, 15, 17, 18, 19, 22, 25]}, ScoreSet {startingPosition=1, scores=[-1, 1, 2, 3, 4, 5, 6, 9, 12, 13, 14, 16, 17, 19, 20]}, ScoreSet {startingPosition=4, scores=[0, 1, 2, 3, 5, 6, 7, 8, 9, 16, 17, 20, 22]}], rightSheet=[ ScoreSet {startingPosition=1, scores=[1, 2, 3, 4, 5, 7, 9, 10, 12, 15, 17, 23, 25]}, ScoreSet {startingPosition=2, scores=[-1, 2, 3, 4, 10, 14, 15, 16, 17, 18, 20]}, ScoreSet {startingPosition=4, scores=[4, 5, 6, 7, 8, 9, 10, 13, 17, 18, 20, 21, 23, 24, 25]}, ScoreSet {startingPosition=5, scores=[-1, 4, 5, 7, 8, 11, 12, 13, 15, 18, 19, 22, 23, 25]}]}

dataSet :: [Game]
dataSet = [Game {serves = 'l', leftPos = 6, leftScore = 0, rightScore = 0, rightPos = 2},Game {serves = 'l', leftPos = 6, leftScore = 1, rightScore = 0, rightPos = 2},Game {serves = 'l', leftPos = 6, leftScore = 2, rightScore = 0, rightPos = 2},Game {serves = 'r', leftPos = 6, leftScore = 2, rightScore = 1, rightPos = 1},Game {serves = 'r', leftPos = 6, leftScore = 2, rightScore = 2, rightPos = 1},Game {serves = 'l', leftPos = 5, leftScore = 3, rightScore = 2, rightPos = 1},Game {serves = 'r', leftPos = 5, leftScore = 3, rightScore = 3, rightPos = 6},Game {serves = 'l', leftPos = 4, leftScore = 4, rightScore = 3, rightPos = 6},Game {serves = 'l', leftPos = 4, leftScore = 5, rightScore = 3, rightPos = 6},Game {serves = 'r', leftPos = 4, leftScore = 5, rightScore = 4, rightPos = 5},Game {serves = 'l', leftPos = 3, leftScore = 6, rightScore = 4, rightPos = 5},Game {serves = 'l', leftPos = 3, leftScore = 7, rightScore = 4, rightPos = 5},Game {serves = 'l', leftPos = 3, leftScore = 8, rightScore = 4, rightPos = 5},Game {serves = 'r', leftPos = 3, leftScore = 8, rightScore = 5, rightPos = 4},Game {serves = 'r', leftPos = 3, leftScore = 8, rightScore = 6, rightPos = 4},Game {serves = 'r', leftPos = 3, leftScore = 8, rightScore = 7, rightPos = 4},Game {serves = 'r', leftPos = 3, leftScore = 8, rightScore = 8, rightPos = 4},Game {serves = 'r', leftPos = 3, leftScore = 8, rightScore = 9, rightPos = 4},Game {serves = 'r', leftPos = 3, leftScore = 8, rightScore = 10, rightPos = 4},Game {serves = 'l', leftPos = 2, leftScore = 9, rightScore = 10, rightPos = 4},Game {serves = 'l', leftPos = 2, leftScore = 10, rightScore = 10, rightPos = 4},Game {serves = 'l', leftPos = 2, leftScore = 11, rightScore = 10, rightPos = 4},Game {serves = 'l', leftPos = 2, leftScore = 12, rightScore = 10, rightPos = 4},Game {serves = 'r', leftPos = 2, leftScore = 12, rightScore = 11, rightPos = 3},Game {serves = 'r', leftPos = 2, leftScore = 12, rightScore = 12, rightPos = 3},Game {serves = 'r', leftPos = 2, leftScore = 12, rightScore = 13, rightPos = 3},Game {serves = 'r', leftPos = 2, leftScore = 12, rightScore = 14, rightPos = 3},Game {serves = 'l', leftPos = 1, leftScore = 13, rightScore = 14, rightPos = 3},Game {serves = 'l', leftPos = 1, leftScore = 14, rightScore = 14, rightPos = 3},Game {serves = 'l', leftPos = 1, leftScore = 15, rightScore = 14, rightPos = 3},Game {serves = 'r', leftPos = 1, leftScore = 15, rightScore = 15, rightPos = 2},Game {serves = 'l', leftPos = 6, leftScore = 16, rightScore = 15, rightPos = 2},Game {serves = 'l', leftPos = 6, leftScore = 17, rightScore = 15, rightPos = 2},Game {serves = 'r', leftPos = 6, leftScore = 17, rightScore = 16, rightPos = 1},Game {serves = 'l', leftPos = 5, leftScore = 18, rightScore = 16, rightPos = 1},Game {serves = 'r', leftPos = 5, leftScore = 18, rightScore = 17, rightPos = 6},Game {serves = 'l', leftPos = 4, leftScore = 19, rightScore = 17, rightPos = 6},Game {serves = 'r', leftPos = 4, leftScore = 19, rightScore = 18, rightPos = 5},Game {serves = 'l', leftPos = 3, leftScore = 20, rightScore = 18, rightPos = 5},Game {serves = 'l', leftPos = 3, leftScore = 21, rightScore = 18, rightPos = 5},Game {serves = 'l', leftPos = 3, leftScore = 22, rightScore = 18, rightPos = 5},Game {serves = 'r', leftPos = 3, leftScore = 22, rightScore = 19, rightPos = 4},Game {serves = 'r', leftPos = 3, leftScore = 22, rightScore = 20, rightPos = 4},Game {serves = 'l', leftPos = 2, leftScore = 23, rightScore = 20, rightPos = 4},Game {serves = 'l', leftPos = 2, leftScore = 24, rightScore = 20, rightPos = 4},Game {serves = 'l', leftPos = 2, leftScore = 25, rightScore = 20, rightPos = 4}]

statSet = [(2,1),(1,6),(1,5),(1,4),(1,3),(2,2),(2,1),(1,6),(2,5),(3,4),(2,3),(6,2),(2,1),(2,6),(1,5),(1,4),(6,3),(4,2),(1,1),(1,6),(1,5),(1,4),(2,3),(5,1),(1,6),(1,5),(1,4),(1,3),(1,2),(1,1),(3,6),(4,5),(1,4),(2,3),(1,2),(2,1),(1,6),(1,5),(4,4),(1,3),(2,2),(1,1),(3,6),(1,5),(1,4),(2,3),(3,2),(1,1),(3,6),(1,5),(2,4)]
-----------------------------------------

nextPos :: Int -> Int
nextPos p
  | p == 1 = 6
  | otherwise = p - 1

prevPos :: Int -> Int
prevPos p
  | p == 6 = 1
  | otherwise = p + 1

startingGame :: ScoreSet -> ScoreSet -> Game
startingGame l r = Game{serves = if head (scores l) == (-1) then 'r' else 'l',
       leftPos = startingPosition l, leftScore = 0, rightScore = 0,
       rightPos = startingPosition r}

addScore :: Char -> Int -> Int -> [Game] -> [Game]
addScore s n p acc
  | n > 0 && s == 'l' = addLeftScore (n-l) (nextPos p) acc
  | n > 0 && s == 'r' = addRightScore (n-r) (nextPos p) acc
  | otherwise = acc
    where l = leftScore $ last acc
          r = rightScore $ last acc

addLeftScore :: Int -> Int -> [Game] -> [Game]
addLeftScore 0 _ acc = acc
addLeftScore n p acc = addLeftScore (n-1) p (acc ++ [Game {serves = 'l', leftPos = p, leftScore = l+1, rightScore = r, rightPos = rp}])
    where l = leftScore $ last acc
          r = rightScore $ last acc
          rp = rightPos $ last acc

addRightScore :: Int -> Int -> [Game] -> [Game]
addRightScore 0 _ acc = acc
addRightScore n p acc = addRightScore (n-1) p (acc ++ [Game {serves = 'r', leftPos = lp, leftScore = l, rightScore = r+1, rightPos = p}])
    where l = leftScore $ last acc
          r = rightScore $ last acc
          lp = leftPos $ last acc

processScores :: ScoreSet -> ScoreSet -> [Game] -> Char -> [Game]
processScores 
  ScoreSet {scores=[]}
  ScoreSet {scores=[]}
  acc
  _ = acc
processScores
  ScoreSet {startingPosition=dummyL, scores=[]}
  ScoreSet {startingPosition=dummyR, scores=(y:ys)}
  acc
  _ = processScores
        ScoreSet {startingPosition=dummyL, scores=[]}
        ScoreSet {startingPosition=dummyR, scores=ys}
        (addScore 'r' y (rightPos $ last acc) acc)
        'l'
processScores
  ScoreSet {startingPosition=dummyL, scores=(x:xs)}
  ScoreSet {startingPosition=dummyR, scores=[]}
  acc
  _ = processScores
        ScoreSet {startingPosition=dummyL, scores=xs}
        ScoreSet {startingPosition=dummyR, scores=[]}
        (addScore 'l' x (leftPos $ last acc) acc)
        'r'
processScores
  ScoreSet {startingPosition=dummyL, scores=((-1):xs)}
  ScoreSet {startingPosition=q, scores=(y:ys)}
  acc
  _ = processScores 
        ScoreSet {startingPosition=dummyL, scores=xs}
        ScoreSet {startingPosition=q, scores=ys}
        (addScore 'r' y (prevPos q) acc)
        'l'
processScores
  ScoreSet {startingPosition=p, scores=(x:xs)}
  ScoreSet {startingPosition=dummyR, scores=((-1):ys)}
  acc
  _ = processScores
        ScoreSet {startingPosition=p, scores=xs}
        ScoreSet {startingPosition=dummyR, scores=ys}
        (addScore 'l' x (prevPos p) acc)
        'r'
processScores
  ScoreSet {startingPosition=dummyL, scores=(x:xs)}
  ScoreSet {startingPosition=dummyR, scores=(y:ys)}
  acc
  s
    | s == 'l' = processScores
                   ScoreSet {startingPosition=dummyL, scores=xs}
                   ScoreSet {startingPosition=dummyR, scores=y:ys}
                   (addScore s x (leftPos $ last acc) acc)
                   'r'
    | otherwise = processScores 
                    ScoreSet {startingPosition=dummyL, scores=x:xs}
                    ScoreSet {startingPosition=dummyR, scores=ys}
                    (addScore s y (rightPos $ last acc) acc)
                    'l'

processSheet :: [ScoreSet] -> [ScoreSet] -> [[Game]] -> [[Game]]
processSheet [] [] acc = acc
processSheet (xs:xss) (ys:yss) acc = processSheet xss yss (acc ++ [processScores xs ys [startingGame xs ys] 'x'])

getServes :: Char -> [Game] -> [Int]
getServes s xs = map leftPos $ filter (\n -> serves n == s) xs

-- run length encoding
sideOuts :: [Int] -> [(Int, Int)]
sideOuts = map rle . group
  where rle xs = (length xs, head xs)

calcStats :: [(Int,Int)] -> Int -> Stats
calcStats xs pos = Stats {sideouts=srv, points=sdt, efficiency=fromIntegral srv/ fromIntegral sdt}
  where
    posStats = filter (\x -> snd x == pos) xs
    srv = length posStats
    sdt = sum $ fst <$> posStats

calcOverall :: [(Int,Int)] -> Stats
calcOverall xs = Stats {sideouts=srv, points=sdt, efficiency=fromIntegral srv/ fromIntegral sdt}
  where
    srv = length xs
    sdt = sum $ fst <$> xs

printStats :: (Int, Stats) -> IO ()
printStats (r, s) = putStrLn $ "Rotation " ++ show r ++ " -> " ++ show s

main :: IO ()
main = do
  [f] <- getArgs
  d <- decode <$> B.readFile f :: IO (Maybe ScoreSheet)
  case d of
    Nothing -> putStrLn "Unable to parse scoresheet"
    Just s -> do
      putStrLn "Sideout stats (the higher efficiency the better):"
      let soStats = concat $ sideOuts <$> (getServes 'r' <$> processSheet (leftSheet s) (rightSheet s) [])
      mapM_ printStats $ zip [1..6] (calcStats soStats <$> [1..6])
      putStrLn $ "Overall sideout: " ++ show(calcOverall soStats)
      putStrLn "Serve stats (the lower efficiency the better):"
      let srStats = concat $ sideOuts <$> (getServes 'l' <$> processSheet (leftSheet s) (rightSheet s) [])
      mapM_ printStats $ zip [1..6] (calcStats srStats <$> [1..6])
      putStrLn $ "Overall serve: " ++ show (calcOverall srStats)
  -- mapM_ print $ processSheet left right []
  -- print $ processScores (left !! 1) (right !! 1) [startingGame (left !! 1) (right !! 1)] 'x'
  -- let acc = [Game {leftPos = 1, leftScore = 0, rightScore = 0, rightPos = 1},Game {leftPos = 1, leftScore = 0, rightScore = 1, rightPos = 6}]
  -- print $ addScore 'l' 1 (leftPos $ last acc) acc
  -- print $ sideOuts [6,6,5,4,3,3,3,3,3,3,2,2,2,2,1,6,5,4,3,3]
  -- print $ sideOuts $ getServes dataSet 'r'
  -- print $ concat $ sideOuts <$> (getServes 'r' <$> processSheet left right [])
  -- mapM_ print $ zip [1..6] (calcStats statSet <$> [1..6])