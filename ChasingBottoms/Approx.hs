{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

module ChasingBottoms.Approx
  ( Approx(..)
  , module ChasingBottoms.Nat
  ) where

import ChasingBottoms.Nat
import Data.Generics
import qualified List

-- For testing:
import ChasingBottoms.IsBottom
import ChasingBottoms.SemanticOrd

{-|
'Approx' is a class for approximation functions as described
in The generic approximation lemma, Graham Hutton and Jeremy
Gibbons, Information Processing Letters, 79(4):197-201, Elsevier
Science, August 2001, <http://www.cs.nott.ac.uk/~gmh/bib.html>.

Instances are provided for all members of the 'Data' type class.  Note
that the implementation is only guaranteed to perform correctly (with
respect to the paper) on polynomial datatypes; in particular, nested
or mutually recursive types are not handled correctly.

In practice the 'approxAll' function can probably be more useful than
'approx'. It traverses down /all/ subterms, and it should be possible
to prove a variant of the approximation lemma which 'approxAll'
satisfies.
-}

class Approx a where
  -- | @'approxAll' n x@ traverses @n@ levels down in @x@ and replaces all
  -- values at that level with bottoms.
  approxAll :: Nat -> a -> a

  -- | 'approx' works like 'approxAll', but the traversal and
  -- replacement is only performed at subterms of the same monomorphic
  -- type as the original term. For polynomial datatypes this is
  -- exactly what the version of @approx@ described in the paper above
  -- does.
  approx :: Nat -> a -> a

instance Data a => Approx a where
  approxAll = approxAllGen
  approx    = approxGen

-- From The generic approximation lemma (Hutton, Gibbons):

-- Generic definition for arbitrary datatype \mu F:
--   approx (n+1) = in . F (approx n) . out

-- Approximation lemma (valid if F is locally continuous),
-- for x, y :: \mu F:
--   x = y  <=>  forall n in Nat corresponding to natural numbers.
--                  approx n x = approx n y

approxGen :: Data a => Nat -> a -> a
approxGen n | n == 0    = error "approx 0 = _|_"
            | otherwise =
  \(x :: a) -> gmapT (mkT (approxGen (pred n) :: a -> a)) x

-- We use mkT to only recurse on the original type. This solution is
-- actually rather nice! But sadly it doesn't work for nested types...

-- Note that the function is defined in the \n -> \x -> style, not
-- \n x -> which would mean something subtly different.

------------------------------------------------------------------------

-- Recurses on everything...

approxAllGen :: Data a => Nat -> a -> a
approxAllGen n | n == 0    = error "approx 0 = _|_"
               | otherwise = \x -> gmapT (approxAllGen (pred n)) x

------------------------------------------------------------------------

-- Behaves exactly like approxGen. (?)

approxGen' :: Data a => Nat -> a -> a
approxGen' n
  | n == 0    = error "approx 0 = _|_"
  | otherwise = \x ->
     let d = dataTypeOf x
         n' = pred n
         fun childTerm = if dataTypeOf childTerm == d then
                           approxGen' n' childTerm
                          else
                           childTerm
     in gmapT fun x

instance Eq DataType where
  d1 == d2 = dataTypeCons d1 =|= dataTypeCons d2

-- (=|=) implements equality on unordered lists. It is comparable in
-- efficiency to (==) if the two lists are equal as lists.
(=|=) :: Eq a => [a] -> [a] -> Bool
xs =|= ys = case xs of
  [] ->    null ys
  x:xs' -> if x `elem` ys then
             xs' =|= List.delete x ys
            else
             False

------------------------------------------------------------------------
-- Tests

-- Improve the testing here. (Use QuickCheck when there is some proper
-- infrastructure for testing bottoms and infinite stuff. Hmm... This
-- module is part of that infrastructure, so be careful.)

data Tree = L | B Tree Tree deriving (Typeable, Data)

leftInfinite = B leftInfinite L
twoLevels    = B (B bottom bottom) L
threeLevels  = B (B (B bottom bottom) L) L

data PerfectTree t = PL t | PB (PerfectTree (t, t))
                     deriving (Show, Typeable, Data)

pTree :: PerfectTree Int
pTree = PB (PB (PL ((1, 2), (3, 4))))

-- Full form of PerfectTree: PT A = Lift (A + PT (Lift (A x A))).
-- Define F G A = Lift (A + G (Lift (A x A))).

type F g a = Either a (g (a, a))

-- Assume that F is locally continuous. We have PT = mu F = F (mu F),
-- i.e. PT a = F PT a, with operations
--  in  :: F PT a -> PT a
--  out :: PT a -> F PT a
--  map :: (forall a . G a -> G' a) -> F G a -> F G' a

in_PT :: F PerfectTree t -> PerfectTree t
in_PT x = case x of
  Left t   -> PL t
  Right tt -> PB tt

out_PT :: PerfectTree t -> F PerfectTree t
out_PT x = case x of
  PL t  -> Left t
  PB tt -> Right tt

map_PT :: (forall t . g t -> g' t) -> F g t -> F g' t
map_PT f x = case x of
  Left t   -> Left t
  Right tt -> Right (f tt)

-- And now we can define approx for this type:

approx_PT :: Nat -> PerfectTree a -> PerfectTree a
approx_PT n | n == 0    = error "approx 0 == _|_"
            | otherwise = in_PT . map_PT (approx_PT (pred n)) . out_PT

-- Some types with several parameters.

data A a b = A0 a | A1 b deriving (Typeable, Data)
data C a b = C0 (C a b) | C1 a b deriving (Typeable, Data)

tests =
    -- approx 0 = bottom.
  [ approx 0 ==! (bottom :: Int -> Int)
  , approx 0 ==! (bottom :: Char -> Char)
  , approx 0 True ==! (bottom :: Bool)

    -- approx (Succ n) /= bottom.
  , approx 1 /=! (bottom :: Int -> Int)
  , approx 1 /=! (bottom :: Char -> Char)

    -- approx n descends n levels.
  , approx 3 "test" ==! "tes" ++ bottom
  , approx 3 "tes"  ==! "tes" ++ bottom
  , approx 3 "te"   ==! "te"
  , approx 3 "t"    ==! "t"

    -- This also works for infinite and multiply branching
    -- structures.
  , approx 2 leftInfinite ==! twoLevels
  , approx 3 leftInfinite ==! threeLevels

    -- Multiple parameter data types shouldn't pose a problem.
  , approx 1 (A0 (A1 True) :: A (A Char Bool) Char) ==! A0 (A1 True)
  , approx 2 (C0 (C1 'a' True)) ==! C0 (C1 'a' True)
  , approx 1 (C0 (C1 'a' True)) ==! C0 bottom

    -- Multiple parameter data types shouldn't pose a problem for
    -- approxAll either.
  , approxAll 1 (A0 (A1 True) :: A (A Char Bool) Char) ==! A0 bottom
  , approxAll 1 (C0 (C1 'a' True)) ==! C0 bottom

    -- approxAll doesn't descend only on the original type...
  , approxAll 1 (Just (Just (Just True))) ==! (Just bottom)

    -- ...but approx does...
  , approx 1 (Just (Just (Just True))) ==! (Just (Just (Just True)))

    -- ...more or less:
  , approx 2 pTree /=! approx_PT 2 pTree
    -- Note that a perfect implementation would have an equality
    -- here.
  ]

testsOK = and tests

-- Due to a staging restriction this doesn't work...
--   $(if not testsOK then fail "Tests failed." else return [])
