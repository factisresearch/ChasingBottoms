{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

-- | Generic semantic equality and order. The implementation is based
-- on 'isBottom', and has the same limitations. Note that non-bottom
-- functions are not handled by any of the functions described below.

module ChasingBottoms.SemanticOrd
  ( SemanticEq(..)
  , SemanticOrd(..)
  , module IsBottom
  ) where

import Data.Generics
import ChasingBottoms.IsBottom
-- import Debug.QuickCheck
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

-- We can't implement compare/min/max. Possible interesting
-- variations: lub, glb :: a -> a -> Maybe a, semanticCompare :: a ->
-- a -> Maybe Ordering.

class SemanticEq a => SemanticOrd a where
  (<!), (<=!), (>=!), (>!) :: a -> a -> Bool

  -- | @'semanticCompare' x y@ returns 'Nothing' if @x@ and @y@ are
  -- incomparable, and @'Just' o@ otherwise, where @o :: 'Ordering'@
  -- represents the relation between @x@ and @y@.
  semanticCompare :: a -> a -> Maybe Ordering

  (\/!) :: a -> a -> Maybe a
  -- | @x '\/!' y@ and @x '/\!' y@ compute the least upper and greatest
  -- lower bounds, respectively, of @x@ and @y@ in the semantical
  -- domain ordering. Note that the least upper bound may not always
  -- exist.
  -- This functionality was implemented just because it was
  -- possible (and to provide analogues of 'max' and 'min' in the 'Ord'
  -- class). If anyone finds any use for it, please let me know.
  (/\!) :: a -> a -> a

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
  (/\!)  = \x y -> x /\!! y
  (\/!)  = \x y -> x \/!! y

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
    -- This should be possible, in some framework, but I don't know
    -- how to do it just yet.
    -- cast' (\x -> cast' a x /\!! cast' b x) :: b
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
          -- This should be possible, in some framework, but I don't
          -- know how to do it.
          -- cast (\x -> cast' a x \/! cast' b x) :: Maybe b
          error "\\/! does not handle non-bottom functions."
      | not (a =^= b)                -> Nothing
      | otherwise                    -> tmapM (\/!!) a b

tmkM :: (Typeable a, Typeable b, Typeable c, Monad m) =>
        (a -> b -> m b) -> a -> c -> m c
tmkM f x y = mkM (f x) y

cast' :: (Typeable a, Typeable b) => a -> b
cast' = Maybe.fromJust . cast
    
------------------------------------------------------------------------

-- TODO: Implement a comparison operator which also works for functions.

-- GenericQ (GenericQ r) = forall a b . (Data a, Data b) => a -> b -> r

-- tmapQl :: (r -> r -> r) -> r -> GenericQ (GenericQ r) -> GenericQ (GenericQ r)

-- everywhere :: (forall a . Data a => a -> a) -> forall a . Data a => a -> a
-- everywhere f = f . gmapT (everywhere f)

-- gmapT :: Data a => (forall b . Data b => b -> b) -> a -> a

-- teverywhere :: (forall a . Data a => a -> a) ->
--                (forall a . Data a => a -> a) ->
--                forall a . Data a => a ->
--                forall a . Data a => a ->
--                a
-- teverywhere 

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
