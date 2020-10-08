module Layout.UIModel where

import Data.Eq (class Eq)
import Data.Functor (class Functor, map, (<$>))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Heterogeneous.Mapping as H
import Heterogeneous.Mapping as HM
import Layout.DomClass (GroupFlag(..))
import Layout.FlexboxModel (FlexDirectionValue(..), Size)
import Layout.FlexboxModel as FM
import Layout.Named (Named, mapNamed)
import Prim.Row (class Nub, class Union)
import Record as R

el :: forall dom. LProp -> Layout dom -> Layout dom
el lp = mapProp (R.merge lp)

rawProp :: forall dom. LProp -> dom -> Layout dom
rawProp = RawProp

raw :: forall dom. dom -> Layout dom
raw = Raw

empty :: forall dom. Layout dom
empty = Empty

---------------------
-- Unkeyed variants

row :: forall dom. LProp -> Array (Layout dom) -> Layout dom
row p c = Group p{along = Just Row, groupType = Just SmallGroupType} c

largeRow :: forall dom. LProp -> Array (Layout dom) -> Layout dom
largeRow p c = Group p{along = Just Row, groupType = Just LargeGroupType} c

col :: forall dom. LProp -> Array (Layout dom) -> Layout dom
col p c = Group p{along = Just Column, groupType = Just SmallGroupType} c

largeCol :: forall dom. LProp -> Array (Layout dom) -> Layout dom
largeCol p c = Group p{along = Just Column, groupType = Just LargeGroupType} c

---------------------
-- Keyed variants

keyedRow :: forall dom. LProp -> Array (Named (Layout dom)) -> Layout dom
keyedRow p c = Keyed p{along = Just Row, groupType = Just SmallGroupType} c

keyedLargeRow :: forall dom. LProp -> Array (Named (Layout dom)) -> Layout dom
keyedLargeRow p c = Keyed p{along = Just Row, groupType = Just LargeGroupType} c

keyedCol :: forall dom. LProp -> Array (Named (Layout dom)) -> Layout dom
keyedCol p c = Keyed p{along = Just Column, groupType = Just SmallGroupType} c

keyedLargeCol :: forall dom. LProp -> Array (Named (Layout dom)) -> Layout dom
keyedLargeCol p c = Keyed p{along = Just Column, groupType = Just LargeGroupType} c

---------------------

-- A spacer node is inserted between elements of start/center/end sections
spacer :: forall dom. Layout dom
spacer = Group (props {groupType: SmallGroupType, grow: FM.GrowValue 1}) []

-- Layout ADT
data Layout dom
  -- Root of the Layout is treated specially
  -- This is useful, for example, to switch screens on mobiles
  = Group LProp (Array (Layout dom)) -- ^ A Group of elements
  | Keyed LProp (Array (Named (Layout dom))) -- ^ A Group of named elements
  | RawProp LProp dom -- ^  A Single wrapped element
  | Raw dom -- ^ An unwrapped raw dom element
  | Empty -- ^ An empty element

instance functorLayout :: Functor Layout where
  map _ Empty = Empty
  map f (Raw dom) = Raw (f dom)
  map f (RawProp p dom) = RawProp p (f dom)
  map f (Keyed p cs) = Keyed p (mapNamed (map f) <$> cs)
  map f (Group p cs) = Group p (map f <$> cs)

mapProp :: forall dom. (LProp -> LProp) -> Layout dom -> Layout dom
mapProp f dom = case dom of
  Group p children -> Group (f p) children
  Keyed p children -> Keyed (f p) children
  RawProp p child -> RawProp (f p) child
  Raw child -> RawProp (f emptyProp) child
  Empty -> Empty

-- Layout Props
-- These are different from (easier to use) flexbox' supported props
type LPropRow' f =
  ( groupType :: f GroupType
  , along :: f FM.FlexDirectionValue
  , wrap :: f FM.WrapValue
  , order :: f Order
  , align :: f FM.JustifyContentValue
  , padding :: f Padding
  , spacing :: f Spacing
  , grow :: f FM.GrowValue
  , shrink :: f FM.ShrinkValue
  , height :: f FM.HeightValue
  , width :: f FM.WidthValue
  )

type LPropRow = LPropRow' Maybe
type LPropRowIdentity = LPropRow' Identity
type LProp' f = Record (LPropRow' f)
type LProp = LProp' Maybe

-- | Easier method to construct prop records
-- | You don't have to provide all fields, or wrap things in Just
-- | To be used like this - props {wrap: WrapNormal, ...}
props :: forall r x s. Union x LPropRow s => Nub s LPropRow => H.HMap MkJust r (Record x) => r -> LProp
props r = R.merge (H.hmap MkJust r) emptyProp

-- Internal
data MkJust = MkJust
instance mappingJustInst :: HM.Mapping MkJust x (Maybe x) where
  mapping MkJust = Just

-- | A Prop that sets nothing
emptyProp :: LProp
emptyProp =
  { groupType: Nothing
  , along: Nothing
  , wrap: Nothing
  , order: Nothing
  , align: Nothing
  , padding: Nothing
  , spacing: Nothing
  , grow: Nothing
  , shrink: Nothing
  , height: Nothing
  , width: Nothing
  }

-- If this element is a group -
-- If Small, then this should have a small number of elements. Usually used for layout
-- If Large, then this is able to handle an arbitrarily large number of elements, but has other limitations.
--   For example, it cannot have ordering, align content, or reverse wrapping. Usually used for main content
data GroupType = SmallGroupType | LargeGroupType
derive instance eqGroupType :: Eq GroupType

toGroupFlag :: GroupType -> GroupFlag
toGroupFlag SmallGroupType = SmallGroupFlag
toGroupFlag LargeGroupType = LargeGroupFlag

-- Order of this child relative to its siblings
type Order = Int

-- Inner padding for this container
type Padding = { xaxis :: Maybe Size, yaxis :: Maybe Size }

-- Spacing between children of this container, across different axes
type Spacing = { xaxis :: Maybe Size, yaxis :: Maybe Size }
