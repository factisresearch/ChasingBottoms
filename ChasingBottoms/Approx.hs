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

-- Type annotations added just for clarity.
map_PT :: (forall t . g t -> g' t) -> F g t -> F g' t
map_PT (f :: forall t . g t -> g' t) (x :: F g t) = case x of
  Left t   -> Left t
  Right tt -> Right ((f :: g (t, t) -> g' (t, t)) tt)

fullMap_PT :: (forall a . a -> a) -> PerfectTree a -> PerfectTree a
fullMap_PT f = in_PT . map_PT (fullMap_PT f) . out_PT

-- And now we can define approx for this type:

approx_PT :: Nat -> PerfectTree a -> PerfectTree a
approx_PT n | n == 0    = error "approx 0 == _|_"
            | otherwise = in_PT . map_PT (approx_PT (pred n)) . out_PT

-- Some types with several parameters.

data A a b = A0 a | A1 b deriving (Typeable, Data)
data C a b = C0 (C a b) | C1 a b deriving (Typeable, Data)

-- -- Mutually recursive types:

data M1 a = M11 a | M12 (M2 a) deriving (Typeable, Data)
data M2 a = M21 (M1 a) | M22 a deriving (Typeable, Data)

{-
 Functor: F (G, H) a = (Lift (a + H a), Lift (G a + a))
          (M1 a, M2 a) = mu F a = F (mu F) a

  in  :: F (mu F) a -> (mu F) a
  out :: (mu F) a -> F (mu F) a

 F is a functor : C^C x C^C -> (C x C)^C.
 (Here C is the base category consisting of monomorphic "Haskell"
 types and functions.)

 Hence given an arrow (natural transformation) : (G, H) -> (G', H')
 (G, G', H and H' objects (functors) in C^C) F yields an arrow
 (natural transformation) : F (G, H) -> F (G', H').

 What is an arrow in C^C x C^C? It is a pair of arrows, both in C^C,
 i.e. two natural transformations on functors in C.

 Hence, a natural transformation : (G, H) -> (G', H') is, for each
 object (type) a, a pair of arrows (functions)
 : (G a -> G' a, H a -> H' a).

 In other words we have
  map :: (forall a . (G a -> G' a, H a -> H' a))
      -> (forall a . F (G, H) a -> F (G', H') a)

 I'm not sure if this is how one usually defines map for a mutually
 recursive type, but it seems reasonable. However, I'm not sure
 exactly how one should go about defining approx using the above
 map. The standard definition from the article isn't applicable.

 Note that I never state in the documentation that mutually recursive
 types are handled incorrectly (since I don't know for sure what the
 correct behaviour is), but I don't think that they are, as the tests
 below show.
-}

-- The following is an approximation, since we don't have proper
-- products.
type F' g h a = (Either a (h a), Either (g a) a)
type Pair g h a = (g a, h a)
type M a = (M1 a, M2 a)

inn' :: F' M1 M2 a -> M a
inn' x = (m1, m2)
  where
  (x1, x2) = x
  m1 = case x1 of
    Left a -> M11 a
    Right m  -> M12 m
  m2 = case x2 of
    Left m  -> M21 m
    Right a -> M22 a

out' :: M a -> F' M1 M2 a
out' m = (x1, x2)
  where
  (m1, m2) = m
  x1 = case m1 of
    M11 a -> Left a
    M12 m -> Right m
  x2 = case m2 of
    M21 m -> Left m
    M22 a -> Right a

map' :: (forall a . (g a -> g' a, h a -> h' a))
        -> F' g h a -> F' g' h' a
map' f (x1, x2) = (x1', x2')
  where
  (g, h) = f
  x1' = case x1 of
    Left a -> Left a
    Right m -> Right (h m)
  x2' = case x2 of
    Right a -> Right a
    Left m  -> Left (g m)

approx_M :: Nat -> M a -> M a
approx_M n | n == 0    = error "approx 0 == _|_"
           | otherwise =
               inn' . map' (approx_M1 (pred n), approx_M2 (pred n)) . out'

approx_M1 :: Nat -> M1 a -> M1 a
approx_M1 n x | n /= 0 = fst (approx_M n (x, undefined))
approx_M2 :: Nat -> M2 a -> M2 a
approx_M2 n x | n /= 0 = snd (approx_M n (undefined, x))

approx1 :: Nat -> M1 a -> M1 a
approx1 n | n == 0    = error "approx 0 == _|_"
          | otherwise = \x -> fst $
  (inn' . map' (approx1 (pred n), approx2 (pred n)) . out') (x, undefined)
approx2 :: Nat -> M2 a -> M2 a
approx2 n | n == 0    = error "approx 0 == _|_"
          | otherwise = \x -> snd $
  (inn' . map' (approx1 (pred n), approx2 (pred n)) . out') (undefined, x)

-- Another variant.
map'' :: (forall a . Pair g h a -> Pair g' h' a) -> F' g h a -> F' g' h' a
map'' f (x1, x2) = (x1', x2')
  where
  x1' = case x1 of
    Left a -> Left a
    Right m -> Right (snd $ f (undefined, m))
  x2' = case x2 of
    Right a -> Right a
    Left m  -> Left (fst $ f (m, undefined))

approx_M' :: Nat -> M a -> M a
approx_M' n | n == 0    = error "approx 0 == _|_"
            | otherwise = inn' . map'' (approx_M' (pred n)) . out'

-- Without the extra test we would have
-- approx1' 0 /=! bottom, but approx1 0 ==! bottom.
approx1' :: Nat -> M1 a -> M1 a
approx1' n | n /= 0 = \x -> fst (approx_M' n (x, undefined))
approx2' :: Nat -> M2 a -> M2 a
approx2' n | n /= 0 = \x -> snd (approx_M' n (undefined, x))

-- Direct definition:
approx_M1' :: Nat -> M1 a -> M1 a
approx_M1' n | n == 0    = error "approx 0 == _|_"
             | otherwise = \m -> case m of
  M11 _ -> m
  M12 m -> M12 (approx_M2' (pred n) m)
approx_M2' :: Nat -> M2 a -> M2 a
approx_M2' n | n == 0    = error "approx 0 == _|_"
             | otherwise = \m -> case m of
  M21 m -> M21 (approx_M1' (pred n) m)
  M22 _ -> m

m1 = M12 (M21 (M12 (M22 'a')))
m2 = M21 (M12 (M21 (M11 'b')))

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
  , approxAll 2 pTree ==! approx_PT 2 pTree
  , approxAll 2 m1 ==! approx1 2 m1

    -- ...but approx does...
  , approx 1 (Just (Just (Just True))) ==! (Just (Just (Just True)))

    -- ...more or less:
  , approx 2 pTree /=! approx_PT 2 pTree
  , approx 2 m1    /=! approx1 2 m1
    -- Note that a perfect implementation would have equalities here.

    -- All the approxes for Ms are equivalent:
  , approx1   3 m1 ==! approx1'   3 m1
  , approx1'  3 m1 ==! approx_M1  3 m1
  , approx_M1 3 m1 ==! approx_M1' 3 m1
  , approx2   3 m2 ==! approx2'   3 m2
  , approx2'  3 m2 ==! approx_M2  3 m2
  , approx_M2 3 m2 ==! approx_M2' 3 m2
  ]

testsOK = and tests

-- Due to a staging restriction this doesn't work...
--   $(if not testsOK then fail "Tests failed." else return [])
