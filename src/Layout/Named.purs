module Layout.Named where

import Data.Tuple (Tuple(..))

-- | A named value
type Named a =
  { name :: String
  , val :: a
  }

named :: forall a. String -> a -> Named a
named s a = {name: s, val: a}

mapNamed :: forall a b. (a -> b) -> Named a -> Named b
mapNamed f n = n {val = f n.val}

namedToTuple :: forall a. Named a -> Tuple String a
namedToTuple x = Tuple x.name x.val
