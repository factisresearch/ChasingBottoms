{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

-- | Generic semantic equality and order. The semantic order referred
-- to is that of a typical CPO for Haskell types, where e.g. @('True',
-- 'bottom') '<=!' ('True', 'False')@, but where @('True', 'True')@
-- and @('True', 'False')@ are incomparable.
--
-- The implementation is based on 'isBottom', and has the same
-- limitations. Note that non-bottom functions are not handled by any
-- of the functions described below.

module ChasingBottoms.SemanticOrd
  ( SemanticEq(..)
  , SemanticOrd(..)
  , module ChasingBottoms.IsBottom
  ) where

import Data.Generics
import ChasingBottoms.IsBottom
import ChasingBottoms.IsType
import qualified Maybe

infix 4 <!, <=!, ==!, >=!, >!, /=!

-- | 'SemanticEq' contains methods for testing whether two terms are
-- semantically equal.

-- Note that we only allow a -> a -> Bool here, not a -> b ->
-- Bool. Otherwise we would allow behaviour like the following:
--   > (bottom : bottom :: [Int]) <=!! ("tr" :: String)
--   True

class SemanticEq a where
  (==!), (/=!) :: a -> a -> Bool

  (/=!) = \x y -> not (x ==! y)

-- | 'SemanticOrd' contains methods for testing whether two terms are
-- related according to the semantic domain ordering.

class SemanticEq a => SemanticOrd a where
  (<!), (<=!), (>=!), (>!) :: a -> a -> Bool

  -- | @'semanticCompare' x y@ returns 'Nothing' if @x@ and @y@ are
  -- incomparable, and @'Just' o@ otherwise, where @o :: 'Ordering'@
  -- represents the relation between @x@ and @y@.
  semanticCompare :: a -> a -> Maybe Ordering

  (\/!) :: a -> a -> Maybe a
  (/\!) :: a -> a -> a
  -- ^ @x '\/!' y@ and @x '/\!' y@ compute the least upper and greatest
  -- lower bounds, respectively, of @x@ and @y@ in the semantical
  -- domain ordering. Note that the least upper bound may not always
  -- exist.
  -- This functionality was implemented just because it was
  -- possible (and to provide analogues of 'max' and 'min' in the 'Ord'
  -- class). If anyone finds any use for it, please let me know.

  (>=!) = flip (<=!)
  (<!)  = \x y -> x <=! y && x /=! y
  (>!)  = \x y -> x >=! y && x /=! y

  semanticCompare x y | x <!  y   = Just LT
                      | x ==! y   = Just EQ
                      | x >!  y   = Just GT
                      | otherwise = Nothing

instance Data a => SemanticEq a where
  (==!) = (==!!)

instance Data a => SemanticOrd a where
  (<=!) = (<=!!)
  (/\!)  = (/\!!)
  (\/!)  = (\/!!)

------------------------------------------------------------------------

type Rel = (Data a, Data b) => a -> b -> Bool

(==!!), (<=!!) :: Rel

a ==!! b = case (isBottom a, isBottom b) of
  (True, True)   -> True
  (False, False) -> allOK (==!!) a b
  _              -> False

a <=!! b = case (isBottom a, isBottom b) of
  (True, _)      -> True
  (False, False) -> allOK (<=!!) a b
  _              -> False

allOK :: Rel -> Rel
allOK op a b =
  -- It's really enough to test just a, since we restrict the types
  -- above, but why complicate things?
  if isFunction a || isFunction b then
    error "The generic versions of (==!) and friends do not accept non-bottom \
          \functions."
   else
    a =^= b && childrenOK op a b

-- Check top-level. Note that this test always fails for "function
-- constructors".
(=^=) :: Rel
a =^= b = toConstr a == toConstr b

-- Check children.
childrenOK :: Rel -> Rel
childrenOK op = tmapQl (&&) True op

------------------------------------------------------------------------

(/\!!) :: (Data a, Data b) => a -> b -> b
a /\!! (b :: b) =
  if isBottom a || isBottom b then
    bottom
   else if isFunction a || isFunction b then
    error "/\\! does not handle non-bottom functions."
   else if not (a =^= b) then
    bottom
   else
    tmapT (/\!!) a b
    
(\/!!) :: (Data a, Data b) => a -> b -> Maybe b
a \/!! (b :: b) =
  case (isBottom a, isBottom b) of
    (True,  True)  -> Just bottom
    (True,  False) -> Just b
    (False, True)  -> cast a
    (False, False)
      | isFunction a || isFunction b ->
          error "\\/! does not handle non-bottom functions."
      | not (a =^= b)                -> Nothing
      | otherwise                    -> tmapM (\/!!) a b

------------------------------------------------------------------------

-- TODO: Implement a comparison operator which also works for functions.

-- f ===! g = case (isBottom f, isBottom g) of
--   (True, True)   -> property True
--   (False, False) -> forAll arbitrary $ \x ->
--     f x ==! g x
--   _              -> property False

------------------------------------------------------------------------
-- Tests

tests =
  [

    -- We should check that ==! corresponds to an equivalence
    -- relation, and so on for the other operators, plus test that
    -- they really do what they are supposed to do.

  ]

testsOK = and tests
