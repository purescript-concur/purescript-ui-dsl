module Layout.FlexboxModel where

import Data.Eq (class Eq)
import Data.Maybe (Maybe(..))

-- Flexbox ADT
-- data FlexDom dom
--   = FlexNode { flexProps :: Array FlexProp
--          , children :: Array (FlexDom dom)
--          }
--   | FlexRaw dom
--   | FlexEmpty

-- Flexbox Props
data FlexProp
  = JustifyContent JustifyContentValue
  | AlignContent AlignContentValue
  | AlignItems AlignItemsValue
  | AlignSelf AlignSelfValue
  | FlexDirection FlexDirectionValue
  | FlexWrap WrapValue
  | FlexGrow GrowValue
  | FlexShrink ShrinkValue
  | FlexOrder OrderValue
  -- | DistributionBasis DistributionBasisValue
  | Margin MarginValue
  | Padding PaddingValue
  | Width WidthValue
  | Height HeightValue

-- The three alignment groups
data Alignment = Start | Center | End
derive instance eqAlignment :: Eq Alignment

-- How to justify the items along the main-axis
-- Left and Right are ignored for now as they don't always make sense
data JustifyContentValue
  = JustifyWith Alignment | SpaceBetween | SpaceAround -- | SpaceEvenly -- | Left | Right
derive instance eqJustifyContentValue :: Eq JustifyContentValue

-- How to justify the *extra* space in the cross-axis
-- This behaves similarly to JustifyContent, but on the cross-axis
type AlignContentValue = JustifyContentValue

-- Layout behaviour on the Cross axis
data AlignItemsValue = AlignWith Alignment | Baseline | Stretch

-- Override align-items behaviour for individual items
type AlignSelfValue = AlignItemsValue

-- Establishes the Main Axis, row (horizontal), or column (vertical).
data FlexDirectionValue = Row | RowReverse | Column | ColumnReverse
derive instance eqFlexDirectionValue :: Eq FlexDirectionValue

-- How should elements be wrapped when they don't fit in one row/column on the main axis
-- Reverse wrapping is not possible for large groups
data WrapValue = Wrap | WrapReverse | NoWrap
derive instance eqWrapValue :: Eq WrapValue

-- Grow a child in proportion to its siblings
-- Grow must be positive
newtype GrowValue = GrowValue Int

-- Shrink a child in proportion to its siblings
-- Shrink must be positive
newtype ShrinkValue = ShrinkValue Int

-- Controls the order in which an element appears inside the parent flex container
newtype OrderValue = OrderValue Int

-- Margin and padding are strictly not flexbox properties
-- But they are important for layout and we need them to
-- be able to control spacing and padding in the UI DSL.
data Size
  = Px Number
  | Rem Number
  | Em Number
derive instance eqSize :: Eq Size

mapSize :: (Number -> Number) -> Size -> Size
mapSize f (Px i) = Px (f i)
mapSize f (Rem i) = Rem (f i)
mapSize f (Em i) = Em (f i)

-- TODO: Make typesafe
type HeightValue = String
type WidthValue = String

type MarginValue
  = { top :: Maybe Size, right :: Maybe Size, bottom :: Maybe Size, left :: Maybe Size }

type PaddingValue
  = { top :: Maybe Size, right :: Maybe Size, bottom :: Maybe Size, left :: Maybe Size }

-- Various useful combinations of margins
margin4 :: Size -> Size -> Size -> Size -> FlexProp
margin4 st sr sb sl = Margin {top: Just st, right: Just sr, bottom: Just sb, left: Just sl}
margin2 :: Size -> Size -> FlexProp
margin2 sy sx = Margin {top: Just sy, right: Just sx, bottom: Just sy, left: Just sx}
margin :: Size -> FlexProp
margin s = Margin {top: Just s, right: Just s, bottom: Just s, left: Just s}
marginX :: Size -> FlexProp
marginX s = Margin {top: Nothing, right: Just s, bottom: Nothing, left: Just s}
marginY :: Size -> FlexProp
marginY s = Margin {top: Just s, right: Nothing, bottom: Just s, left: Nothing}
marginTop :: Size -> FlexProp
marginTop s = Margin {top: Just s, right: Nothing, bottom: Nothing, left: Nothing}
marginRight :: Size -> FlexProp
marginRight s = Margin {top: Nothing, right: Just s, bottom: Nothing, left: Nothing}
marginBottom :: Size -> FlexProp
marginBottom s = Margin {top: Nothing, right: Nothing, bottom: Just s, left: Nothing}
marginLeft :: Size -> FlexProp
marginLeft s = Margin {top: Nothing, right: Nothing, bottom: Nothing, left: Just s}

-- Various useful combinations of paddings
padding4 :: Size -> Size -> Size -> Size -> FlexProp
padding4 st sr sb sl = Padding {top: Just st, right: Just sr, bottom: Just sb, left: Just sl}
padding2 :: Size -> Size -> FlexProp
padding2 sy sx = Padding {top: Just sy, right: Just sx, bottom: Just sy, left: Just sx}
padding :: Size -> FlexProp
padding s = Padding {top: Just s, right: Just s, bottom: Just s, left: Just s}
paddingX :: Size -> FlexProp
paddingX s = Padding {top: Nothing, right: Just s, bottom: Nothing, left: Just s}
paddingY :: Size -> FlexProp
paddingY s = Padding {top: Just s, right: Nothing, bottom: Just s, left: Nothing}
paddingTop :: Size -> FlexProp
paddingTop s = Padding {top: Just s, right: Nothing, bottom: Nothing, left: Nothing}
paddingRight :: Size -> FlexProp
paddingRight s = Padding {top: Nothing, right: Just s, bottom: Nothing, left: Nothing}
paddingBottom :: Size -> FlexProp
paddingBottom s = Padding {top: Nothing, right: Nothing, bottom: Just s, left: Nothing}
paddingLeft :: Size -> FlexProp
paddingLeft s = Padding {top: Nothing, right: Nothing, bottom: Nothing, left: Just s}

-- This will determine how grow attribute affects size
-- (Uses the flex-basis property)
--
-- AllSpace will size the entire box according to grow
-- [  short  ][loooooong][      short      ]
-- <----1----><----1----><--------2-------->
--
-- ExtraSpace will size the extra space according to grow
-- [  short  ][  loooooong  ][      short      ]
-- <1>     <1><1>         <1><--2-->     <--2-->
--
-- The other values for flex-basis are poorly supported
-- data DistributionBasisValue = AllSpace {- auto -} | ExtraSpace {- 0 -}

-- A simple effectful typed model for flexbox
-- Nothing unsets the value, or sets it to the default
-- class Flexbox dom where
--   setFlexContainer :: forall a. dom a -> dom a
--   setFlexProp :: forall a. Maybe FlexProp -> dom a -> dom a
