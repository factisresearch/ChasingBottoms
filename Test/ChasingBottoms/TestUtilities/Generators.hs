{-# LANGUAGE RankNTypes, DeriveDataTypeable #-}

-- | Generators that are part of the testing framework.

-- This module contains _many_ generators. Maybe they should be
-- exported, not just from this module, but from the library as well.

module Test.ChasingBottoms.TestUtilities.Generators
  ( -- * Basic types and functions
    Cogen
  , function
  , NotEqualGen
  , GreaterEqualGen
  , JoinableGen
    -- ** @Bool@ generators
  , bool
  , coBool
  , neBool
  , geBool
  , joinBool
    -- ** @Integer@ generators
  , integer
  , coInteger
  , neInteger
  , geInteger
  , joinInteger
    -- ** @[]@ generators
  , finiteListOf
  , infiniteListOf
  , coListOf
  , neListOf
  , geListOf
  , joinListOf
    -- ** @Tree@ generators
  , Tree(..)
  , finiteTreeOf
  , infiniteTreeOf
  , coTreeOf
  , neTreeOf
  , geTreeOf
  , joinTreeOf
    -- * Tests of the generators
  , tests
  ) where

import Test.ChasingBottoms.IsBottom
import Test.ChasingBottoms.SemanticOrd
import Test.ChasingBottoms.TestUtilities
import Test.QuickCheck
import Data.Generics
import Control.Monad
import Data.Maybe

import Test.ChasingBottoms.ApproxShow
import Test.ChasingBottoms.Nat

------------------------------------------------------------------------
-- Data types

-- | Binary trees with information in the leaves.

data Tree a
   = Branch (Tree a) (Tree a)
   | Leaf a
     deriving (Show, Typeable, Data)

------------------------------------------------------------------------
-- Basic generators

integer :: Gen Integer
integer = frequency [ (1, return bottom), (10, arbitrary) ]

bool :: Gen Bool
bool = elements [bottom, False, True]

finiteListOf :: Gen a -> Gen [a]
finiteListOf gen = sized finList
  where
  finList size | size == 0 = baseCase
               | otherwise =
                   frequency [ (1, baseCase)
                             , (10, do elem <- gen
                                       l <- finList (size - 1)
                                       return (elem : l)
                               )
                             ]

  baseCase = elements [bottom, []]

finiteTreeOf :: Gen a -> Gen (Tree a)
finiteTreeOf gen = sized finTree
  where
  finTree size | size == 0 = baseCase
               | otherwise =
                   frequency [ (1, baseCase)
                             , (2, do left  <- finTree (size `div` 2)
                                      right <- finTree (size `div` 2)
                                      return (Branch left right)
                               )
                             ]

  baseCase = frequency [(1, bottom), (3, liftM Leaf gen)]

-- | Definitely infinite lists.

infiniteListOf :: Gen a -> Gen [a]
infiniteListOf gen = liftM2 (:) gen (infiniteListOf gen)

-- | Possibly infinite trees.

infiniteTreeOf :: Gen a -> Gen (Tree a)
infiniteTreeOf gen = infTree
  where
  infTree = frequency [ (1, return bottom)
                      , (1, liftM Leaf gen)
                      , (3, liftM2 Branch infTree infTree)
                      ]

testGen :: (Show a, Data a) => Nat -> Gen a -> IO ()
testGen depth gen = quickCheck $ forAll gen $ \n ->
                      collect (approxShow depth n) $ True

------------------------------------------------------------------------
-- Cogenerators

-- | A mapping from an argument to a generator transformer, like the
-- 'coarbitrary' function.
--
-- Note that the functions generated by the cogenerators in this
-- module are not necessarily monotone.

type Cogen a = forall b. a -> Gen b -> Gen b

coBool :: Cogen Bool
coBool b | isBottom b = variant 0
coBool False          = variant 1
coBool True           = variant 2

coInteger :: Cogen Integer
coInteger i | isBottom i = variant 0
            | otherwise  = variant 1 . coarbitrary i

coListOf :: Cogen a -> Cogen [a]
coListOf cog xs | isBottom xs = variant 0
coListOf cog []               = variant 1
coListOf cog (x:xs)           = variant 2 . cog x . coListOf cog xs

coTreeOf :: Cogen a -> Cogen (Tree a)
coTreeOf cog xs | isBottom xs = variant 0
coTreeOf cog (Leaf x)         = variant 1 . cog x
coTreeOf cog (Branch l r)     = variant 2 . coTreeOf cog l . coTreeOf cog r

-- | Given a 'Cogen' and a 'Gen', generate a function.

-- Note that the functions generated by 'promote' below are all
-- non-bottom.

function :: Cogen a -> Gen b -> Gen (a -> b)
function coGen gen = frequency [ (1, return bottom)
                               , (50, promote (\a -> coGen a gen))
                               ]

testFunction :: (Data a, Data b) => Nat -> Cogen a -> Gen b -> [a] -> IO ()
testFunction depth coGen gen inputs =
  quickCheck $ forAll (function coGen gen) $ \f ->
    collect (map (\x -> approxShow depth (x, f x)) inputs) $ True

------------------------------------------------------------------------
-- Generators for element not equal to argument

-- | Mapping from argument to generator of elements not equal to
-- argument.

type NotEqualGen a = a -> Gen a

neBool :: NotEqualGen Bool
neBool b | isBottom b = elements [False, True]
neBool False          = elements [bottom, True]
neBool True           = elements [bottom, False]

neInteger :: NotEqualGen Integer
neInteger i | isBottom i = arbitrary
            | otherwise  =
                frequency [ (1, return bottom)
                          , (10, do j <- arbitrary
                                    let j' = if j >= 0 then j + 1 else j - 1
                                    return (i + j')
                            )
                          ]

neListOf :: Gen a -> NotEqualGen a -> (Gen a -> Gen [a]) -> NotEqualGen [a]
neListOf gen neg listOf xs = neList xs
  where
  neList xs
    | isBottom xs = frequency [ (1, return []), (10, nonEmpty gen) ]
    | otherwise = case xs of
        []     -> frequency [ (1, return bottom), (10, nonEmpty gen) ]
        (y:ys) -> frequency [ (1, return bottom)
                            , (1, return [])
                            , (5, nonEmpty (neg y))
                            , (5, do y' <- neg y
                                     return (y':ys)
                              )
                            , (5, do ys' <- neList ys
                                     return (y:ys')
                              )
                            ]

  nonEmpty headGen = do x <- headGen
                        xs <- listOf gen
                        return (x:xs)

neTreeOf :: Gen a
         -> NotEqualGen a
         -> (Gen a -> Gen (Tree a))
         -> NotEqualGen (Tree a)
neTreeOf gen neg treeOf t = neTree t
  where
  neTree t
    | isBottom t = frequency [ (1, leaf gen), (10, node) ]
    | otherwise = case t of
        Leaf x     -> frequency [ (1, smallTreeNE x), (2, node) ]
        Branch l r -> frequency [ (1, return bottom)
                                , (2, leaf gen)
                                , (2, do l' <- neTree l
                                         return (Branch l' r)
                                  )
                                , (2, do r' <- neTree r
                                         return (Branch l r')
                                  )
                                , (2, do l' <- neTree l
                                         r' <- node
                                         return (Branch l' r')
                                  )
                                , (2, do l' <- node
                                         r' <- neTree r
                                         return (Branch l' r')
                                  )
                                ]

  leaf g = liftM Leaf g
  smallTreeNE x = frequency [(1, return bottom), (3, leaf (neg x))]

  node = do l <- treeOf gen
            r <- treeOf gen
            return (Branch l r)

prop_notEqualGen element gen =
  forAll (pair element gen) $ \(x, y) ->
    x /=! y

testGenPair :: (Show a, Data a) => Nat -> Gen a -> (a -> Gen a) -> IO ()
testGenPair depth gen gen' = quickCheck $
  forAll (pair gen gen') $ \(x, y) ->
    collect (approxShow depth (x, y)) $ True

------------------------------------------------------------------------
-- Generators for element greater than or equal to argument

-- | Mapping from argument to generator of elements greater than or
-- equal to argument.

type GreaterEqualGen a = a -> Gen a

-- | 'GreaterEqualGen' for flat CPOs.

flatGEGen :: Gen a -> GreaterEqualGen a
flatGEGen gen x | isBottom x = gen
                | otherwise  = return x

geBool :: GreaterEqualGen Bool
geBool = flatGEGen bool

geInteger :: GreaterEqualGen Integer
geInteger = flatGEGen integer

geListOf :: Gen a
         -> GreaterEqualGen a
         -> (Gen a -> Gen [a])
         -> GreaterEqualGen [a]
geListOf gen geGen listOf xs
  | isBottom xs = listOf gen
  | otherwise   = case xs of
      []   -> return []
      y:ys -> do y'  <- geGen y
                 ys' <- geListOf gen geGen listOf ys
                 return (y':ys')

geTreeOf :: Gen a
         -> GreaterEqualGen a
         -> (Gen a -> Gen (Tree a))
         -> GreaterEqualGen (Tree a)
geTreeOf gen geGen treeOf t
  | isBottom t = treeOf gen
  | otherwise  = case t of
      Leaf x   -> liftM Leaf (geGen x)
      Branch l r -> do l' <- geTreeOf gen geGen treeOf l
                       r' <- geTreeOf gen geGen treeOf r
                       return (Branch l' r')

prop_greaterEqualGen element gen =
  forAll (pair element gen) $ \(x, y) ->
    x <=! y

------------------------------------------------------------------------
-- Generators for pairs whose components' join exists

-- | Mapping from argument to generator of elements whose join with
-- the argument is likely to exist.
--
-- Note that the meet of these elements is also likely to be
-- \"interesting\".

type JoinableGen a = a -> Gen a

-- | 'JoinableGen' for flat CPOs.

flatJoinGen :: Gen a -> JoinableGen a
flatJoinGen gen x | isBottom x = gen
                  | otherwise  = frequency [(1, return bottom), (4, return x)]

joinBool :: JoinableGen Bool
joinBool = flatJoinGen bool

joinInteger :: JoinableGen Integer
joinInteger = flatJoinGen integer

joinListOf :: Gen a -> JoinableGen a -> (Gen a -> Gen [a]) -> JoinableGen [a]
joinListOf gen joinGen listOf xs
  | isBottom xs = listOf gen
  | otherwise   = case xs of
      []   -> frequency [(1, return bottom), (4, return [])]
      y:ys -> frequency [ (1, return bottom)
                        , (10, do y'  <- joinGen y
                                  ys' <- joinListOf gen joinGen listOf ys
                                  return (y':ys')
                          )
                        ]

joinTreeOf :: Gen a
           -> JoinableGen a
           -> (Gen a -> Gen (Tree a))
           -> JoinableGen (Tree a)
joinTreeOf gen joinGen treeOf t
  | isBottom t = treeOf gen
  | otherwise   = case t of
      Leaf x   -> frequency [(1, return bottom), (4, liftM Leaf (joinGen x))]
      Branch l r -> frequency [ (1, return bottom)
                              , (5, do l' <- joinTreeOf gen joinGen treeOf l
                                       r' <- joinTreeOf gen joinGen treeOf r
                                       return (Branch l' r')
                                )
                              ]

prop_joinableGen element gen =
  forAll (pair element gen) $ \(x, y) ->
    isJust (x \/! y)

------------------------------------------------------------------------

-- | All tests collected together.

tests :: IO Bool
tests = runQuickCheckTests $ map run theTests
  where
  theTests =
    [ prop_notEqualGen bool neBool
    , prop_notEqualGen integer neInteger
    , prop_notEqualGen (finiteListOf bool)
                       (neListOf bool neBool finiteListOf)
    , prop_notEqualGen (finiteTreeOf integer)
                       (neTreeOf integer neInteger finiteTreeOf)
    , prop_greaterEqualGen bool geBool
    , prop_greaterEqualGen integer geInteger
    , prop_greaterEqualGen (finiteListOf bool)
                           (geListOf bool geBool finiteListOf)
    , prop_greaterEqualGen (finiteTreeOf integer)
                           (geTreeOf integer geInteger finiteTreeOf)
    , prop_joinableGen bool joinBool
    , prop_joinableGen integer joinInteger
    , prop_joinableGen (finiteListOf bool)
                       (joinListOf bool joinBool finiteListOf)
    , prop_joinableGen (finiteTreeOf integer)
                       (joinTreeOf integer joinInteger finiteTreeOf)
    ]
