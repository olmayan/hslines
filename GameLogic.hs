module GameLogic (
    Field (Field),
    emptyField,
    colors,
    side,
    onField,
    countFree,
    throwBall,
    throwBalls,
    path
    ) where

import Control.Monad
import Data.Array
import Data.Tuple (swap)
import System.Random.Mersenne.Pure64

-- Debug-only usage
import Debug.Trace
import Data.List (intercalate)

data Field = Field (Array (Int, Int) Int)

emptyField :: Field
emptyField = Field (array bounds [(point, 0) | point <- range bounds]) where
    bounds = ((0, 0), (side - 1, side - 1))

showField :: Field -> String
showField (Field field) = intercalate "\n"
    (map (\y -> (intercalate [] $ map
        (\x -> let s = show (field ! (x, y))
               in replicate (4 - length s) ' ' ++ s)
        (range (0, 8))))
        (range (0, 8))) ++ "\n"

colors :: Int
colors = 6

side :: Int
side = 9

onField :: (Int, Int) -> Bool
onField (x, y) = x >= 0 && y >= 0 && x < side && y < side

countFree :: Field -> Int
countFree (Field field) = length . filter ( == 0) $ elems field

throwBall :: Field -> PureMT -> (Field, PureMT)
throwBall (Field field) g = (Field field', g') where
    free    = map fst $ filter ((0 == ) . snd) $ assocs field
    numFree = length free
    (n, g') = randomInt g
    at      = n `mod` numFree
    color   = n `div` numFree `mod` colors + 1
    field'  = field // [(free !! at, color)]

throwBalls :: Field -> Int -> PureMT -> (Field, PureMT)
throwBalls field 0 g = (field, g)
throwBalls field n g = throwBalls field' (n - 1) g' where
    (field', g') = throwBall field g


path :: Field -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
path (Field field) from to =
    if to `elem` surrounding from
       then [to]
       else hasPath (field // [(from, -1)]) (-1) [from] where
    surrounding (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]
    hasPath field step points
        | null free = []
        | any (`elem` free) (surrounding to)
              = drawPath (field' // [(to, step - 2)]) [to]
        | otherwise = hasPath field' (step - 1) free
        where
            free = map fst $ filter f $ assocs field
            f (pt, a) = (a == 0) && (any (`elem` points) (surrounding pt))
            field' = (//) field $ map (\pt -> (pt, step - 1)) free
    drawPath field points@(x:_)
        | step < -2 = drawPath field (x':points)
        | otherwise = points
        where
            step = field ! x
            x' = head $ filter (\pt -> (field ! pt) == (step + 1))
                               (filter onField $ surrounding x)
