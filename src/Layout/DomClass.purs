module Layout.DomClass where

import Layout.FlexboxModel (FlexProp)
import Layout.Named (Named)

--------------------------------
-- | The External interface | --
--------------------------------

-- | Flexbox Dom interface
class FlexDom dom where
  -- | Set props for a dom element
  setProps :: Array FlexProp -> dom -> dom
  -- | Create an unkeyed group with specified children
  mkGroup :: GroupFlag -> Array FlexProp -> Array dom -> dom
  -- | Create a keyed group with specified children
  mkKeyedGroup :: GroupFlag -> Array FlexProp -> Array (Named dom) -> dom
  -- | Create a text node
  text :: String -> dom
  -- | Create an empty node
  emptyDom :: dom

-- The ONLY non-flexbox property we require. It's an optimisation
-- This is because small and large groups often need to be treated differently in backends
-- If this element is a group -
-- If Small, then this should have a small number of elements. Usually used for layout
-- If Large, then this is able to handle an arbitrarily large number of elements, but has other limitations.
--   For example, it cannot have ordering, align content, or reverse wrapping. Usually used for main content
data GroupFlag
  = SmallGroupFlag
  -- ^ The element to be used for small groups
  | LargeGroupFlag
  -- ^ The element to be used for large groups
