{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -cpp #-}

-- |
-- Module      :  Test.ChasingBottoms.SemanticOrd
-- Copyright   :  (c) Nils Anders Danielsson 2004, 2005
-- License     :  See the file LICENCE.
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
-- Some functions take the implicit parameters @?approxDepth@ and
-- @?timeOutLimit@. They have the following meaning:
--
-- [@'?approxDepth'@] If equal to @'Just' n@, an @'approxAll' n@ is
-- performed on all arguments before doing whatever the function is
-- supposed to be doing.
--
-- [@'?timeOutLimit'@] If equal to @'Just' n@, then all computations
-- that take more than @n@ seconds to complete are considered to be
-- equal to 'bottom'. This functionality is implemented using
-- 'isBottomTimeOut'.
--
-- One could imagine using QuickCheck for testing equality of
-- functions, but I have not managed to tweak the type system so that
-- it can be done transparently.

module Test.ChasingBottoms.SemanticOrd
  ( SemanticEq(..)
  , SemanticOrd(..)
  ) where

import Data.Generics
import Test.ChasingBottoms.IsBottom
import Test.ChasingBottoms.IsType
import qualified Data.Maybe as Maybe
import Test.ChasingBottoms.Nat
import Test.ChasingBottoms.Approx

infix 4 <!, <=!, ==!, >=!, >!, /=!
infix 4 <?, <=?, ==?, >=?, >?, /=?

-- | 'SemanticEq' contains methods for testing whether two terms are
-- semantically equal.

-- Note that we only allow a -> a -> Bool here, not a -> b ->
-- Bool. Otherwise we would allow behaviour like the following:
--   > (bottom : bottom :: [Int]) <=!! ("tr" :: String)
--   True

class SemanticEq a where
  (==!), (/=!) :: a -> a -> Bool
  (==?), (/=?) :: ( ?approxDepth :: Maybe Nat
                  , ?timeOutLimit :: Maybe Int
                  ) => a -> a -> Bool

  (/=?) = \x y -> not (x ==? y)

  (/=!) = \x y -> not (x ==! y)

  (==!) = bindImpl (==?)

-- | 'SemanticOrd' contains methods for testing whether two terms are
-- related according to the semantic domain ordering.

class SemanticEq a => SemanticOrd a where
  (<!), (<=!), (>=!), (>!) :: a -> a -> Bool
  (<?), (<=?), (>=?), (>?) ::
    ( ?approxDepth :: Maybe Nat
    , ?timeOutLimit :: Maybe Int
    ) => a -> a -> Bool

  semanticCompare :: a -> a -> Maybe Ordering
  semanticCompare' :: ( ?approxDepth :: Maybe Nat
                      , ?timeOutLimit :: Maybe Int
                      ) => a -> a -> Maybe Ordering
  -- ^ @'semanticCompare' x y@ returns 'Nothing' if @x@ and @y@ are
  -- incomparable, and @'Just' o@ otherwise, where @o :: 'Ordering'@
  -- represents the relation between @x@ and @y@.

  (\/!) :: a -> a -> Maybe a
  (/\!) :: a -> a -> a
  (\/?) :: ( ?approxDepth :: Maybe Nat
           , ?timeOutLimit :: Maybe Int
           ) => a -> a -> Maybe a
  (/\?) :: ( ?approxDepth :: Maybe Nat
           , ?timeOutLimit :: Maybe Int
           ) => a -> a -> a
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

  (>=?) = flip (<=?)
  (<?)  = \x y -> x <=? y && x /=? y
  (>?)  = \x y -> x >=? y && x /=? y

  (<=!) = bindImpl (<=?)

  semanticCompare = bindImpl semanticCompare'

  semanticCompare' x y | x <?  y   = Just LT
                       | x ==? y   = Just EQ
                       | x >?  y   = Just Prelude.GT
                       | otherwise = Nothing

  x <=? y = case semanticCompare' x y of
    Just LT -> True
    Just EQ -> True
    _       -> False

  (\/!) = bindImpl (\/?)

  (/\!) = bindImpl (/\?)

instance Data a => SemanticEq a where
  (==?) = liftAppr (==??)

instance Data a => SemanticOrd a where
  (<=?) = liftAppr (<=??)
  (/\?)  = liftAppr (/\??)
  (\/?)  = liftAppr (\/??)

liftAppr op x y = appr x `op` appr y
  where appr = maybe id approxAll ?approxDepth

-- Non-trivial type...

bindImpl ::
  (forall . (?approxDepth :: Maybe Nat, ?timeOutLimit :: Maybe Int) => a) -> a
bindImpl f = let ?approxDepth = Nothing
                 ?timeOutLimit = Nothing
             in f

------------------------------------------------------------------------

type Rel = (?timeOutLimit :: Maybe Int, Data a, Data b) => a -> b -> Bool

(==??), (<=??) :: Rel

a ==?? b = case (isBottomTimeOut a, isBottomTimeOut b) of
  (True, True)   -> True
  (False, False) -> allOK (==??) a b
  _              -> False

a <=?? b = case (isBottomTimeOut a, isBottomTimeOut b) of
  (True, _)      -> True
  (False, False) -> allOK (<=??) a b
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
childrenOK op = foldr (&&) True .|.. gzipWithQ op
  where f .|.. g = \x y -> f (g x y)

------------------------------------------------------------------------

(/\??) :: (?timeOutLimit :: Maybe Int, Data a, Data b) => a -> b -> b
a /\?? (b :: b) =
  if isBottomTimeOut a || isBottomTimeOut b then
    bottom
   else if isFunction a || isFunction b then
    error "/\\! does not handle non-bottom functions."
   else if not (a =^= b) then
    bottom
   else
    gzipWithT (/\??) a b
    
(\/??) :: (?timeOutLimit :: Maybe Int, Data a, Data b) => a -> b -> Maybe b
a \/?? (b :: b) =
  case (isBottomTimeOut a, isBottomTimeOut b) of
    (True,  True)  -> Just bottom
    (True,  False) -> Just b
    (False, True)  -> cast a
    (False, False)
      | isFunction a || isFunction b ->
          error "\\/! does not handle non-bottom functions."
      | not (a =^= b)                -> Nothing
      | otherwise                    -> gzipWithM (\/??) a b

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
-- Compatibility functions

#if __GLASGOW_HASKELL__ <= 602

gzipWithT :: GenericQ GenericT -> GenericQ GenericT
gzipWithT = tmapT
gzipWithM :: Monad m => GenericQ (GenericM m) -> GenericQ (GenericM m)
gzipWithM = tmapM
gzipWithQ :: GenericQ (GenericQ r) -> GenericQ (GenericQ [r])
gzipWithQ q = tmapQl (++) [] (\x y -> [q x y])

#endif

------------------------------------------------------------------------
-- Tests

tests =
  [

    -- We should check that ==! corresponds to an equivalence
    -- relation, and so on for the other operators, plus test that
    -- they really do what they are supposed to do.

  ]

testsOK = and tests
