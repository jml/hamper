module Util (double, doUntil, mapToFlat, flatToMap) where

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


doUntil predicate action =
    do result <- action
       if predicate result
          then return [result]
          else do results <- (doUntil predicate action)
                  return (result : results)

doWhile predicate action = doUntil (not. predicate) action


double f (x, y) = (f(x), f(y))
