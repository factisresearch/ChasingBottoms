{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

-- |
-- Module      :  ChasingBottoms.SemanticOrd
-- Copyright   :  (c) Nils Anders Danielsson 2004
-- License     :  See the file LICENSE.
-- 
-- Maintainer  :  http://www.cs.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (GHC-specific)
--
-- Generic semantic equality and order. The semantic order referred
-- to is that of a typical CPO for Haskell types, where e.g. @('True',
-- 'bottom') '<=!' ('True', 'False')@, but where @('True', 'True')@
-- and @('True', 'False')@ are incomparable.
--
-- The implementation is based on 'isBottom', and has the same
-- limitations. Note that non-bottom functions are not handled by any
-- of the functions described below.
--
-- One could imagine using QuickCheck for testing equality of
-- functions, but I have not managed to tweak the type system so that
-- it can be done transparently.

module ChasingBottoms.SemanticOrd
  ( SemanticEq(..)
  , SemanticOrd(..)
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
    -- cast' a `fop` cast' b
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

-- Variant of cast.

-- cast' :: (Typeable a, Typeable b) => a -> b
-- cast' = Maybe.fromJust . cast

------------------------------------------------------------------------

-- TODO: Implement a comparison operator which also works for functions.

-- newtype EqFun = EqFun { unEqFun ::
--   forall a b . (Data a, Data b) => a -> b -> Bool }

-- class SemanticFunEq a where
--   (!==!), (!/=!) :: a -> a -> Bool

--   (!/=!) = \x y -> not (x !==! y)

-- instance Data a => SemanticFunEq a where
--  x !==! y =
--   let test :: (Arbitrary b, Show b, Data c) =>
--               (b -> c1) -> (b -> c2) -> Bool
--       test f g = testIt (forAll arbitrary $ \(x :: b) -> f x !==!! g x)
--   in let ?funTest = EqFun test
--   in x !==!! y

-- (!==!!) :: (Data a, Data b, ?funTest :: EqFun) => a -> b -> Bool
-- x !==!! y = case (isBottom x, isBottom y) of
--   (True, True)   -> True
--   (False, False) | isFunction x -> unEqFun ?funTest x y
--                  | otherwise -> x =^= y && tmapQl (&&) True (!==!!) x y
--   _              -> False

-- This one works, but it only handles functions on the top level, not
-- functions inside e.g. lists.

-- instance (Show a, Arbitrary a, SemanticFunEq b) => SemanticFunEq (a -> b) where
--   f !==! g = case (isBottom f, isBottom g) of
--     (True, True)   -> True
--     (False, False) -> testIt (forAll arbitrary $ \x -> f x !==! g x)
--     _              -> False

-- instance SemanticEq a => SemanticFunEq a where
--   a !==! b = case (isBottom a, isBottom b) of
--     (True, True)   -> True
--     (False, False) -> -- We know that we are not dealing with functions.
--                       a ==! b
--     _              -> False

------------------------------------------------------------------------
-- Tests

tests =
  [

    -- We should check that ==! corresponds to an equivalence
    -- relation, and so on for the other operators, plus test that
    -- they really do what they are supposed to do.

  ]

testsOK = and tests
