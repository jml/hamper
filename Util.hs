module Util (double, doUntil, mapToFlat, flatToMap, showMap, mapItems, transformMap) where

import Data.List
import qualified Data.Map as Map

toFlatList :: [(a, a)] -> [a]
toFlatList ((x, y) : xs) = x : y : toFlatList xs
toFlatList [] = []

fromFlatList :: (Ord a) => [a] -> [(a, a)]
fromFlatList (x : y : xs) = (x, y) : fromFlatList xs
fromFlatList [] = []

mapToFlat = toFlatList . Map.toList

flatToMap :: (Ord a) => [a] -> (Map.Map a a)
flatToMap = Map.fromList . fromFlatList

transformMap :: (Ord a, Ord c) => (a -> c) -> (b -> d) -> Map.Map a b -> Map.Map c d
transformMap f g m = Map.fromList [(f a, g b) | (a, b) <- Map.toList m]


mapItems :: (Ord a) => ((a, b) -> x) -> Map.Map a b -> [x]
mapItems f = (map f) . Map.toList


showMap datamap =
    let showPair (k, v) = show k ++ " => " ++ show v in
    intercalate ", " (mapItems showPair datamap)

doUntil predicate action =
    do result <- action
       if predicate result
          then return [result]
          else do results <- (doUntil predicate action)
                  return (result : results)

doWhile predicate action = doUntil (not. predicate) action


double f (x, y) = (f x, f y)
