module Layout.Render where

import Control.Applicative (pure)
import Control.Bind ((>>=))
import Data.Array as A
import Data.Boolean (otherwise)
import Data.Eq ((==))
import Data.EuclideanRing ((-), (/))
import Data.Function (($), (<<<))
import Data.Functor (map)
import Data.Maybe (Maybe(..), maybe)
import Layout.DomClass (class FlexDom, GroupFlag(..), emptyDom, mkGroup, mkKeyedGroup, setProps)
import Layout.FlexboxModel (Alignment(..), FlexProp(..), JustifyContentValue(..), Size(..), mapSize, padding2, paddingX, paddingY)
import Layout.Named (Named, mapNamed)
import Layout.UIModel (LProp, Layout(..), emptyProp, spacer, toGroupFlag)

-- This is the crux. Layout compilation to native dom.
render :: forall dom. FlexDom dom => Layout dom -> dom
render c = case c of
  Group p cs ->
    let renderedChildren = map render (separateChildren cs)
        groupFlag = maybe SmallGroupFlag toGroupFlag p.groupType
    in mkGroup groupFlag (flexProps p) (setMargins p setProps renderedChildren)
  Keyed p cs ->
    let renderedChildren = map (mapNamed render) (separateKeyed cs)
        groupFlag = maybe SmallGroupFlag toGroupFlag p.groupType
    in mkKeyedGroup groupFlag (flexProps p) (setMargins p (\fp -> mapNamed (setProps fp)) renderedChildren)
  RawProp p child -> setProps (flexProps p) child
  Raw dom -> dom
  Empty -> emptyDom

-- For a list of children
-- Set margin left/right = 1/2 of parent spacing
-- TODO: This first/last-child thing may not be needed
-- For the first child we set margin-left/top to 0, and for the last child we set margin-right/bottom to 0
-- setMargins :: forall dom. FlexDom dom => LProp -> Array dom -> Array dom
setMargins :: forall a b. LProp -> (Array FlexProp -> a -> b) -> Array a -> Array b
setMargins lp setProps cs =
  let spacingx = map (mapSize (_ / 2.0)) $ lp.spacing >>= _.xaxis
      spacingy = map (mapSize (_ / 2.0)) $ lp.spacing >>= _.yaxis
      max = A.length cs - 1
      zero = Px 0.0
      marginSet i
        | i == 0 = setProps [Margin {top: Just zero, right: spacingx, bottom: spacingy, left: Just zero}]
        | i == max = setProps [Margin {top: spacingy, right: Just zero, bottom: Just zero, left: spacingx}]
        | otherwise = setProps [Margin {top: spacingy, right: spacingx, bottom: spacingy, left: spacingx}]
  in A.mapWithIndex marginSet cs

-- | Make flexbox props from layoutprops
flexProps :: LProp -> Array FlexProp
flexProps lp = A.concat
  [ paddingProps
  , widthProps
  , heightProps
  , wrapProps
  , directionProps
  , growProps
  , shrinkProps
  ]
  where
    directionProps = maybe [] (pure <<< FlexDirection) lp.along
    wrapProps = maybe [] (pure <<< FlexWrap) lp.wrap
    growProps = maybe [] (pure <<< FlexGrow) lp.grow
    shrinkProps = maybe [] (pure <<< FlexShrink) lp.shrink
    -- TODO: We don't use space-around/between
    -- JustifyProps don't need to be rendered
    -- Those are automatically handled by the separate* functions below
    -- justifyProps = maybe [] (pure <<< JustifyContent) lp.align
    widthProps = maybe [] (pure <<< Width) lp.width
    heightProps = maybe [] (pure <<< Height) lp.height
    paddingProps =
      let paddingx = lp.padding >>= _.xaxis
          paddingy = lp.padding >>= _.yaxis
      in
        case paddingx, paddingy of
          Nothing, Nothing -> []
          Just px, Just py -> [padding2 px py]
          Just px, Nothing -> [paddingX px]
          Nothing, Just py -> [paddingY py]

-- | Separate keyed children
separateKeyed :: forall dom. Array (Named (Layout dom)) -> Array (Named (Layout dom))
separateKeyed = separateChildrenGeneral namedSpacer \x -> (getLayoutProps x.val).align
  where
    namedSpacer = { name: "spacer", val: spacer }

-- | Separate unkeyed children
separateChildren :: forall dom. Array (Layout dom) -> Array (Layout dom)
separateChildren = separateChildrenGeneral spacer \x -> (getLayoutProps x).align

-- For a list of children -
-- For each alignment change from start->center->end, we add a sep item with flex-grow:1.
-- This could be generalised to more than 3 groups (left, center, end), but we don't do it right now.
separateChildrenGeneral :: forall e. e -> (e -> Maybe JustifyContentValue) -> Array e -> Array e
separateChildrenGeneral spacer alignment cs = A.concat
  [ left
  , withSep center
  , withSep right
  ]
  where
    left = leftAll.init
    center = centerRight.init
    right = centerRight.rest
    leftAll = A.span (aligned (JustifyWith Start)) cs
    centerRight = A.span (aligned (JustifyWith Center)) leftAll.rest
    aligned s x = maybe true (_ == s) (alignment x)
    withSep [] = []
    withSep x = A.cons spacer x

getLayoutProps :: forall dom. Layout dom -> LProp
getLayoutProps d = case d of
  Group lp _ -> lp
  RawProp lp _ -> lp
  _ -> emptyProp

{-
/* An illustration of Row behaviour */

/* A Note of elm-ui behaviour for padding/spacing.
Elm-UI doesn't use border-box. Let's say the spacing was set to 40px. Then -
It instead sets the container width to - calc(100% + 40px).
And gives it a margin of - margin: 0 -20px. Where 20px = 40px/2.
This does the same thing as below, and does NOT require the :first-child and :last-child settings.
But I prefer doing border-box since -
1. That's cleaner
2. Calc CSS properties may not be supported everywhere.
2. That's what Yoga layout does.
*/

<div class="container">
    <div class="child red-child"></div>
    <div class="child green-child"></div>
    <div class="sep"></div>
    <div class="child yellow-child"></div>
    <div class="child red-child"></div>
    <div class="sep"></div>
    <div class="child"></div>
</div>

.sep {
    flex-grow: 1;
}

.container {
    /* All containers should have display:flex */
    display: flex;
    width: 100%;
    flex-wrap: wrap;
    margin: 0px auto;
    background-color: #BDBDBD;
    align-content: center;
    /* Padding for containers works as expected with border-box */
    padding: 0.5em;
    /* Containers should have border-box box-sizing. This makes the padding independent of box size.
       This is also followed by Yoga layout */
    box-sizing: border-box;
}

.container > :first-child {
    /* FIRST child should zero the LEFT margin to make sure the container padding is used */
    margin-left: 0;
}

.container > :last-child {
    /* LAST child should zero the RIGHT margin to make sure the container padding is used */
    margin-right: 0;
}

.child {
    min-width: 5em;
    min-height: 5em;
    margin: 0em 0.2em;
    border-radius: 1em;
    background-color: lightblue;
}

.red-child {
    background-color: darkred;
}
.green-child {
    background-color: darkgreen;
}
.yellow-child {
    background-color: khaki;
}

-}
