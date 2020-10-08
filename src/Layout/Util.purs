module Util where

import Data.Array as A
import Data.Maybe (Maybe(..))

mapHead :: forall a. (a -> a) -> Array a -> Array a
mapHead f arr = case A.uncons arr of
  Nothing -> arr
  Just x -> A.cons (f x.head) x.tail

mapTail :: forall a. (a -> a) -> Array a -> Array a
mapTail f arr = case A.unsnoc arr of
  Nothing -> arr
  Just x -> A.snoc x.init (f x.last)
