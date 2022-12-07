module Main (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortBy)
import Debug.Trace (trace, traceShow)
import Text.Printf (printf)
import System.Random (getStdGen, randomR, RandomGen())
import System.Random.Shuffle (shuffle')

data Pupil = Pupil String Int Int
    deriving (Eq, Show)

data Slot = Slot Int Int
    deriving Eq

instance Show Slot where
    show (Slot d n) = printf "Day %d, %02d:%02d" d h m
        where h = 8 + n `div` 2
              m = 30 * (n `mod` 2)

data Type = Prep | Exam
    deriving (Eq, Show)

data AssignedSlot = ASlot Type Int Int
    deriving Eq

instance Show AssignedSlot where
    show (ASlot t d n) = show (Slot d n) ++ " (" ++ show t ++ ")"

instance Ord AssignedSlot where
    (ASlot t d n) <= (ASlot t' d' n') = (d, n) <= (d', n')

-- examination period: 8:00 - 16:00 - makes for (16-8) * 2 = 16 slots
-- the first 2 are unavailable (prep takes at least 2 slots)
-- the lunch slot (12-8)*2 = 8 is unavailable
slots = [ Slot d n | d <- [1..], n <- [0 .. 15], n >= 2, n /= 8 ]

validSlot (ASlot _ _ n) = n >= 0 && n <= 15

parsePupil :: String -> Pupil
parsePupil ('*':name) = Pupil name 3 1
parsePupil name       = Pupil name 2 1

makeInitialDist :: RandomGen gen => gen -> [Pupil] -> [Pupil]
makeInitialDist gen ps = shuffle' ps (length ps) gen

resolveSlots :: Slot -> Pupil -> [AssignedSlot]
resolveSlots (Slot d n) (Pupil _ x y) =
    [ ASlot Prep d (n-i) | i <- [1 .. x] ] ++
    [ ASlot Exam d (n+i) | i <- [0 .. y-1] ]

overRoomCount :: [AssignedSlot] -> Int
overRoomCount ss = sum $ map (\n -> n - 3) over4s
    where addToRoomCount slot m = M.alter increment slot m
          increment Nothing  = Just 1
          increment (Just n) = Just (n+1)
          over4s = M.elems $ M.filter (>= 4) $ foldr addToRoomCount M.empty ss

scoreDist :: [Pupil] -> Int
scoreDist ps = invalidSlotPenalty + overRoomPenalty
    where studentSlots = concatMap (uncurry resolveSlots) $ zip slots ps
          invalidSlotPenalty =
            100 * length (filter (not . validSlot) studentSlots)
          overRoomPenalty = 30 * overRoomCount studentSlots

mutate gen dist =
    if i1 == i2 then mutate gen'' dist
    else (gen'', map ((dist !!) . swap) [0 .. ld-1])
    where (i1, gen') = randomR (0, ld-1) gen
          (i2, gen'') = randomR (0, ld-1) gen'
          ld = length dist
          swap n | n == i1   = i2
                 | n == i2   = i1
                 | otherwise = n

optimizeDist gen 0         bestDist = (bestDist, gen)
optimizeDist gen bestScore bestDist =
    if score < bestScore
    then trace ("Score improved from " ++ show bestScore ++ " to " ++ show score)
                   optimizeDist gen' score dist'
    else optimizeDist gen' bestScore bestDist
        where (gen', dist') = mutate gen bestDist
              score         = scoreDist dist'

showSolution pupils = mapM_ (uncurry3 showTime) $ reverse students
    where showTime slot pupil@(Pupil name _ _) prepRoom =
            putStrLn $ show prep ++ "\t" ++
                       "Room " ++ show prepRoom ++ "\t" ++
                       show slot ++ "\t" ++
                       name
                where prep = minimum $ filter isPrep $ resolveSlots slot pupil

          isPrep (ASlot Prep _ _) = True
          isPrep _                = False

          uncurry3 f (a, b, c) = f a b c

          assignPrepRoom :: ([(Slot, Pupil, Int)], [S.Set AssignedSlot]) ->
                            (Slot, Pupil) ->
                            ([(Slot, Pupil, Int)], [S.Set AssignedSlot])
          assignPrepRoom (ps, rs) (slot, pupil) = assignPrepRoom' 1 rs
              where preps = S.fromList $ filter isPrep $ resolveSlots slot pupil

                    pize n = (slot, pupil, n) : ps

                    assignPrepRoom' n [] = (pize n, [preps])
                    assignPrepRoom' n (r:rs) =
                        if S.null $ S.intersection r preps
                        then (pize n, S.union r preps : rs)
                        else let (p, rs') = assignPrepRoom' (n+1) rs
                             in (p, r : rs')

          cmpMinPrep (s, p) (s', p') = compare rs rs'
            where rs  = minimum $ filter isPrep $ resolveSlots s p
                  rs' = minimum $ filter isPrep $ resolveSlots s' p'
          studentSlots = sortBy cmpMinPrep $ zip slots pupils

          (students, prs) = foldl assignPrepRoom ([],[]) studentSlots


main :: IO ()
main = do
    input <- fmap (map parsePupil . lines) getContents
    gen <- getStdGen
    let initialDist = makeInitialDist gen input
    let initialScore = scoreDist initialDist
    let (solution, gen') = optimizeDist gen initialScore initialDist
    showSolution solution
