module Data.RdsData.Internal.Maybe
  ( toMaybe
  ) where

toMaybe :: a -> Bool -> Maybe a
toMaybe x True = Just x
toMaybe _ _ = Nothing
