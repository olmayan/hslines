module GameLogic (
    Field (Field),
    field,
    colors,
    side,
    throwBall,
    throwBalls,
    hasPath
    ) where

import Control.Monad
import Data.Array
import Data.Tuple (swap)
import System.Random.Mersenne.Pure64

data Field = Field (Array (Int, Int) Int)

field :: Field
field = Field (array bounds [(point, 0) | point <- range bounds]) where
    bounds = ((0, 0), (side - 1, side - 1))

colors :: Int
colors = 6

side :: Int
side = 9

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

hasPath :: Field -> (Int, Int) -> (Int, Int) -> Bool
hasPath (Field field) from@(x1, y1) to@(x2, y2) =
    if to `elem` (from : surrounding from) then True
                                           else hasPath' (field // [(from, -1)]) [from] where
    surrounding (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]
    hasPath' field points
        | null free = False
        | any (`elem` free) (surrounding to) = True
        | otherwise = hasPath' field' free
        where
            free = map fst $ filter f $ assocs field
            f (pt, a) = a == 0 && any (`elem` points) (surrounding pt)
            field' = (//) field $ map (\pt -> (pt, (-1))) free

main = putStrLn "Hello, World!"