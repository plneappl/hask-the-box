{-# OPTIONS -Wno-incomplete-patterns #-}

module Util.FourOf 
  ( FourOf(..)
  , repeated
  , iter
  , fromList
  ) where

data FourOf a = FourOf a a a a deriving (Eq, Ord, Show)

repeated :: a -> FourOf a
repeated it = FourOf it it it it

iter :: FourOf a -> [a]
iter (FourOf a b c d) = [a, b, c, d]

fromList :: [a] -> FourOf a
fromList (a:b:c:d:_) = FourOf a b c d

instance Functor FourOf where
  fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)