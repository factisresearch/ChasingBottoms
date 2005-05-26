{-# OPTIONS -fglasgow-exts -cpp #-}

-- | Tests of everything related to "Test.ChasingBottoms". Not finished
--   yet. (Missing: "SemanticOrd".)
--
-- Note that the warnings given when compiling this module are
-- intentional. See the internal comments for more information.

module Test.ChasingBottoms.Tests(tests) where

#if __GLASGOW_HASKELL__ <= 602
import Debug.QuickCheck
#else
import Test.QuickCheck
#endif
import Text.Show.Functions
import Test.ChasingBottoms.Approx
import Test.ChasingBottoms.ApproxShow
import Test.ChasingBottoms.IsBottom
import Test.ChasingBottoms.TimeOut as T
import Test.ChasingBottoms.SemanticOrd
import Test.ChasingBottoms.Nat
import Test.ChasingBottoms.IsType
import Data.Generics
import System.IO.Unsafe
import Data.Array
import System.Exit
import Data.List
import Data.Ratio
import qualified Control.Exception as E

------------------------------------------------------------------------
-- Tests of the functions in "Test.ChasingBottoms.Approx"

-- Improve the testing here. (Use QuickCheck when there is some proper
-- infrastructure for testing bottoms and infinite stuff. Hmm... This
-- module is part of that infrastructure, so be careful.)

data Tree = Lf | Br Tree Tree deriving (Typeable, Data)

leftInfinite = Br leftInfinite Lf
twoLevels    = Br (Br bottom bottom) Lf
threeLevels  = Br (Br (Br bottom bottom) Lf) Lf

-- A nested type:
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

-- Pattern type signature added just for clarity.
map_PT :: (forall t . g t -> g' t) -> F g t' -> F g' t'
map_PT f x = case x of
  Left t                 -> Left t
  Right (tt :: g (t, t)) -> Right (f tt)

-- Note that we get a boring map using this kind of functor for nested
-- types:
fullMap_PT :: (forall a . a -> a) -> PerfectTree a -> PerfectTree a
fullMap_PT f = in_PT . map_PT (fullMap_PT f) . out_PT

-- And now we can define approx for this type:

approx_PT :: Nat -> PerfectTree a -> PerfectTree a
approx_PT n | n == 0    = error "approx 0 == _|_"
            | otherwise = in_PT . map_PT (approx_PT (pred n)) . out_PT

-- Some types with several parameters.
data A a b = A0 a | A1 b deriving (Typeable, Data)
data C a b = C0 (C a b) | C1 a b deriving (Typeable, Data)

-- Mutually recursive types:
data G a = G1 a | G2 (H a) deriving (Typeable, Data)
data H a = H1 (G a) | H2 a deriving (Typeable, Data)

{-

GH a (r1, r2) = (Lift (a + r2), Lift (r1 + a))

G a = fst (mu (GH a))
H a = snd (mu (GH a))

--> is used for arrows in the product category:
(a1, a2) --> (b1, b2) = (a1 -> b1, a2 -> b2)

in  :: GH a (mu (GH a)) --> mu (GH a)
out :: GH a (mu (GH a)) <-- mu (GH a)

map_GH :: (p1 --> p2) -> (GH a p1 --> GH a p2)

-}

-- The following is an approximation, since we don't have proper
-- products. However, if no one breaks the abstraction this won't be a
-- problem.

-- Sadly I cannot write "type GH a (r1, r2) = (Either a r2, Either r1 a)".
type GH a r1 r2 = (Either a r2, Either r1 a)
type M a = (G a, H a)
-- And "type (a1, a2) :--> (b1, b2) = (a1 -> b1, a2 -> b2)" doesn't
-- work either...
type ProdArr a1 a2 b1 b2 = (a1 -> b1, a2 -> b2)

-- in_GH :: GH a (M a) :--> M a
in_GH :: ProdArr (Either a (H a)) (Either (G a) a) (G a) (H a)
in_GH = (in_G, in_H)
  where
  in_G e = case e of
    Left  a -> G1 a
    Right m -> G2 m
  in_H e = case e of
    Left  m -> H1 m
    Right a -> H2 a

-- out_GH :: M a :--> GH a (M a)
out_GH :: ProdArr (G a) (H a) (Either a (H a)) (Either (G a) a)
out_GH = (out_G, out_H)
  where
  out_G m = case m of
    G1 a -> Left  a
    G2 m -> Right m
  out_H m = case m of
    H1 m -> Left  m
    H2 a -> Right a

-- map_GH :: ((a1, a2) :--> (b1, b2)) -> (GH a a1 a2 :--> GH a b1 b2)
map_GH :: ProdArr a1 a2 b1 b2 -> ProdArr (Either a a2) (Either a1 a)
                                         (Either a b2) (Either b1 a)
map_GH gh = (map_G, map_H)
  where
  (g, h) = gh
  map_G e = case e of
    Left  a  -> Left  a
    Right a2 -> Right (h a2)
  map_H e = case e of
    Left  a1 -> Left  (g a1)
    Right a  -> Right a

(.*.) :: ProdArr b1 b2 c1 c2 -> ProdArr a1 a2 b1 b2 -> ProdArr a1 a2 c1 c2
(g1, h1) .*. (g2, h2) = (g1 . g2, h1 . h2)

-- approx_GH :: Nat -> (M a :--> M a)
approx_GH :: Nat -> ProdArr (G a) (H a) (G a) (H a)
approx_GH n | n == 0    = error "approx 0 == _|_"
            | otherwise = in_GH .*. map_GH (approx_GH (pred n)) .*. out_GH

approx_G :: Nat -> G a -> G a
approx_G = fst . approx_GH

approx_H :: Nat -> H a -> H a
approx_H = snd . approx_GH

g1 = G2 (H1 (G2 (H2 'a')))
h1 = H1 (G2 (H1 (G1 'b')))

approxTests =
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
  , approxAll 2 g1 ==! approx_G 2 g1

    -- ...but approx does...
  , approx 1 (Just (Just (Just True))) ==! (Just (Just (Just True)))

    -- ...more or less:
  , approx 2 pTree /=! approx_PT 2 pTree
  , approx 2 g1    /=! approx_G 2 g1
    -- Note that a perfect implementation would have equalities here.
  ]

approxTestsOK = and approxTests

-- Due to a staging restriction this doesn't work...
--   $(if not approxTestsOK then fail "Tests failed." else return [])

------------------------------------------------------------------------
-- Tests of the functions in "Test.ChasingBottoms.TimeOut".

-- The "Micro" variants are not tested directly, but they are used
-- internally by the functions below.

timeOutTests = do
  r1 <- timeOut n bottom
  r1b <- timeOut n $ return bottom
  r2 <- timeOut' n bottom
  r3 <- timeOut n $ return list
  r4 <- timeOut' n list
  r5 <- timeOut n $ return $ reverse list
  r6 <- timeOut' n $ reverse list
  let result = case (r1, r1b, r2, r3, r4, r5, r6) of
       ( Exception _, Value b, Exception _, Value xs, Value ys
        , Value _nt, T.NonTermination)
         -> isBottom b && xs =~= list && ys =~= list
       _ -> False
  return result
  where n = 1
        list = [1..] :: [Integer]
        xs =~= ys = appr xs ==! appr ys
        appr = approxAll 20


------------------------------------------------------------------------
-- Tests of the functions in "Test.ChasingBottoms.IsBottom".

isException f = unsafePerformIO $
  (E.evaluate f >> return False) `E.catch` const (return True)

bot = bot
notbot x = notbot x

data T' a = L' | B' (T' a) (T' a) deriving Eq

instance Monad T' where

leftInfinite' = B' leftInfinite' L'

infiniteRecursion = leftInfinite' == leftInfinite'

data A2 = A2 { aaa :: A2 } | C { ccc :: A2 }

isBottomTests =
    -- Basic cases.
  [ isBottom bottom  ==  True
  , isBottom undefined  ==  True
  , isBottom (error "...")  ==  True
    -- This sometimes leads to a stack overflow.
    -- , isBottom bot  ==  True

    -- const bottom /= bottom.
  , isBottom notbot  ==  False
  , isBottom (const bottom)  ==  False

    -- Other types also lifted.
  , isBottom (bottom, bottom)  ==  False
  , isBottom (Just bottom)  ==  False

    -- Pattern match failure.
  , isBottom (let (x, y) = bottom in x :: Bool)  ==  True
  , isBottom (let Just x = Nothing in x :: Char)  ==  True

    -- Nonterminating, but not bottom.
  , isBottom [1..]  ==  False

    -- Missing methods.
    -- Skip this test to avoid compiler warnings.
  , (isBottom (L' >> L'))  ==  True

    -- Array stuff.
  , isBottom (array (1,0) [] ! 0)  ==  True
  , isBottom (array (0,0) [] ! 0)  ==  True

    -- Record stuff.
    -- Skip the first one to avoid compiler warnings.
  , isBottom (let x = A2 {} in aaa x)  ==  True
  , isBottom (let x = A2 { aaa = x } in ccc x)  ==  True
  , isBottom (let x = A2 { aaa = x } in x { ccc = x })  ==  True

    -- Infinite recursion, no data produced, should yield stack
    -- overflow...
    -- Not a quick test (on some machines, anyway). And the result
    -- might be optimisation dependent.
    -- , isException (isBottom infiniteRecursion)  ==  True

    -- Some other exceptions that are not caught.
  , isException (isBottom (unsafePerformIO $ exitWith ExitSuccess))  ==  True
  , isException (isBottom (1 `div` 0))  ==  True
  ]

------------------------------------------------------------------------
-- Tests of the functions in "Test.ChasingBottoms.ApproxShow".

data T = L | B T T deriving (Typeable, Data)

left = B left L

data Q a = Q a ::: a | Q deriving (Typeable, Data)

pr n x template = do
  let s = approxShow n x
  putStr $ show (s == template)
  putStr " |"
  putStr s
  putStrLn "|"

tst n x template = approxShow n x == template

approxShowTests =
  [ tst 4 left "B (B (B (B _ _) L) L) L"
  , tst 4 (bottom :: Bool) "_|_"
  , tst 4 not "<function /= _|_>"
  , tst 4 ('a','b') "('a', 'b')"
  , tst 1 ('a','b') "(_, _)"
  , tst 4 (Q ::: 'a' ::: 'b' ::: 'c') "((Q ::: 'a') ::: 'b') ::: 'c'"
  , tst 2 (Q ::: 'a' ::: 'b' ::: 'c') "(_ ::: _) ::: 'c'"
  , tst 4 "abc" "\"abc\""
  , tst 4 [True, False, False] "[True, False, False]"
  , tst 2 "abc" "\"a_"
  , tst 2 [True, False, False] "[True, _"
  , tst 1 "" "\"\""
  , tst 1 ([] :: [Bool]) "[]"
  , tst 0 "" "_"
  , tst 0 ([] :: [Bool]) "_"
  , tst 4 ('a' : bottom : bottom) "\"a_|__|_"
  , tst 4 ('a' : bottom : bottom : []) "\"a_|__|_\""
  , tst 4 [True, bottom] "[True, _|_]"
  , tst 4 (True : bottom : bottom) "[True, _|__|_"
  , tst 4 (bottom ::: bottom ::: 'b' ::: 'c') "((_|_ ::: _|_) ::: 'b') ::: 'c'"
  , tst 2 ('a' : bottom : bottom) "\"a_"
  , tst 2 [True, bottom] "[True, _"
  , tst 2 (True : bottom : bottom) "[True, _"
  , tst 2 (bottom ::: bottom ::: 'b' ::: 'c') "(_ ::: _) ::: 'c'"
  ]


------------------------------------------------------------------------
-- Tests of the functions in "Test.ChasingBottoms.IsType".

isTypeTests =
    -- isFunction identifies functions.
  [ isFunction (id :: Char -> Char)  ==  True
  , isFunction ((==) :: Char -> Char -> Bool)  ==  True
  , isFunction 'c'  ==  False
  , isFunction [not]  ==  False

  , isTuple [not]  ==  False
  , isTuple ()  ==  False
  , isTuple ('a', 'c')  ==  True

  , isList ""  ==  True
  , isList [not]  ==  True
  , isList ('a', 'c')  ==  False

  , isString ""  ==  True
  , isString [not]  ==  False
  , isString ('a', 'c')  ==  False
  ]

------------------------------------------------------------------------
-- Tests of the functions in "Test.ChasingBottoms.Nat".

-- Testing isSucc.

prop_isSucc n = isSucc n == (n > 0)

-- Testing fromSucc.

prop_fromSucc n | n == 0    = fromSucc n == Nothing
                | otherwise = fromSucc n == Just (n-1)

-- Testing natrec.

-- How do you test something as versatile as natrec? Well, at least we
-- can verify that we can use it to implement addition.

prop_natrec_add m n = natrec m (\_ o -> succ o) n == m + n

-- There is no need to test foldN, since it is specified by its
-- definition.

-- Testing Enum.

prop_Nat_Enum_succ (n :: Nat) = succ n == n + 1
prop_Nat_Enum_pred (n :: Nat) = n > 0 ==> pred n == n - 1

-- Testing Eq.

prop_Nat_Eq_refl (m :: Nat) = m == m
prop_Nat_Eq_sym (m :: Nat) n =
  -- collect (m, n) $  -- OK distribution.
    m == n ==> n == m
prop_Nat_Eq_trans (i :: Nat) j k =
  -- collect (i, j, k) $  -- Distribution good enough.
    i == j && j == k ==> i == k
prop_Nat_Eq_cong (f :: Nat -> Nat) m n =
  -- collect (m, n) $  -- OK distribution.
    m == n ==> f m == f n

prop_Nat_Eq_noteq (m :: Nat) n = (m == n) == not (m /= n)

-- Testing Show.

prop_Nat_Show (m :: Nat) = show m == show (toInteger m)

-- Testing Ord.

prop_Nat_Ord_refl (m :: Nat) = m <= m
prop_Nat_Ord_antisym (m :: Nat) n =
  -- collect (m, n) $  -- OK distribution.
    m <= n && n <= m ==> m == n
prop_Nat_Ord_trans (i :: Nat) j k =
  -- collect (i, j, k) $  -- OK distribution.
    i <= j && j <= k ==> i <= k

-- Testing Num.

prop_Nat_mul_iterated_sum (m :: Nat) n =
  m * n == foldr (+) 0 (genericReplicate m n)

prop_Nat_plus_assoc (m :: Nat) n o = m + (n + o) == (m + n) + o
prop_Nat_plus_comm (m :: Nat) n    = m + n == n + m

prop_Nat_mul_assoc (m :: Nat) n o = m * (n * o) == (m * n) * o
prop_Nat_mul_comm (m :: Nat) n    = m * n == n * m

prop_Nat_mul_plus_left_dist (m :: Nat) n o = m * (n + o) == m * n + m * o

prop_Nat_mul_plus_zero (m :: Nat) = m + 0 == m
prop_Nat_mul_mul_unit (m :: Nat)  = m * 1 == m

prop_Nat_minus (m :: Nat) n =
  -- collect (m, n) $  -- OK distribution.
  m >= n ==> (m - n) + n == m

prop_Nat_signum_abs (m :: Nat) = signum m * abs m == m
prop_Nat_signum_zero           = signum 0 == 0

prop_Nat_fromInteger_plus m n =
  m >= 0 && n >= 0 ==>
    fromInteger m + fromInteger n == (fromInteger (m + n) :: Nat)
prop_Nat_fromInteger_mul m n =
  m >= 0 && n >= 0 ==>
    fromInteger m * fromInteger n == (fromInteger (m * n) :: Nat)

-- negate is undefined.

-- Testing Integral.

prop_Nat_to_from (m :: Nat) = fromInteger (toInteger m) == m
prop_Nat_from_to i = i >= 0 ==> toInteger (fromInteger i :: Nat) == i

prop_Nat_quotRem (m :: Nat) n =
  n /= 0 ==> m `quotRem` n == (m `quot` n, m `rem` n)
prop_Nat_divMod (m :: Nat) n =
  n /= 0 ==> m `divMod` n == (m `div` n, m `mod` n)

prop_Nat_quot_rem (m :: Nat) n =
  n /= 0 ==> (m `quot` n) * n + m `rem` n == m
prop_Nat_div_mod (m :: Nat) n =
  n /= 0 ==> (m `div` n) * n + m `mod` n == m

-- Testing Real.

prop_Nat_toRational (m :: Nat) = toRational m == toInteger m % 1

-- All tests collected together.

natTests = do
  test prop_isSucc
  test prop_fromSucc
  test prop_natrec_add
  test prop_Nat_Enum_succ
  test prop_Nat_Enum_pred
  test prop_Nat_Eq_refl
  test prop_Nat_Eq_sym
  test prop_Nat_Eq_trans
  test prop_Nat_Eq_cong
  test prop_Nat_Eq_noteq
  test prop_Nat_Show
  test prop_Nat_Ord_refl
  test prop_Nat_Ord_antisym
  test prop_Nat_Ord_trans
  test prop_Nat_mul_iterated_sum
  test prop_Nat_plus_assoc
  test prop_Nat_plus_comm
  test prop_Nat_mul_assoc
  test prop_Nat_mul_comm
  test prop_Nat_mul_plus_left_dist
  test prop_Nat_mul_plus_zero
  test prop_Nat_mul_mul_unit
  test prop_Nat_minus
  test prop_Nat_signum_abs
  test prop_Nat_signum_zero
  test prop_Nat_fromInteger_plus
  test prop_Nat_fromInteger_mul
  test prop_Nat_to_from
  test prop_Nat_from_to
  test prop_Nat_quotRem
  test prop_Nat_divMod
  test prop_Nat_quot_rem
  test prop_Nat_div_mod
  test prop_Nat_toRational

------------------------------------------------------------------------
-- All the tests

-- | This function runs all the tests. The tests have succeeded if it
-- only prints out 'True' a couple of times.

tests :: IO ()
tests = do
  natTests
  print approxTestsOK
  print $ and isBottomTests
  print $ and approxShowTests
  print $ and isTypeTests
  timeOutTests >>= print