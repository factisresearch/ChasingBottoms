{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

-- | Functions for converting arbitrary (non-function, partial,
-- possibly infinite) values into strings.

module ChasingBottoms.ApproxShow(Prec, ApproxShow(..), module Nat) where

import Data.Generics
import ChasingBottoms.IsBottom
import ChasingBottoms.Nat
import ChasingBottoms.IsType
import qualified List

-- | Precedence level.
type Prec = Int

class ApproxShow a where
  -- | @'approxShowsPrec' n@ works like 'showsPrec' with the following
  -- differences:
  --
  --   * After @n@ levels of descent into a term the output is
  --     replaced by @\"_\"@.
  --
  --   * All detectable occurences of bottoms are replaced by @\"_|_\"@.
  --
  --   * Functions are displayed as @\"\<function\>\"@.
  approxShowsPrec :: Nat -> Prec -> a -> ShowS
  approxShows     :: Nat -> a -> ShowS
  approxShow      :: Nat -> a -> String

  approxShows n a = approxShowsPrec n 0 a
  approxShow n a  = approxShowsPrec n 0 a ""

instance Data a => ApproxShow a where
  approxShowsPrec n p = gShowsPrec False n p

-- This is a gigantic hack (due to special treatment of lists and
-- strings). Now I realise how I should have written it:
--   A wrapper taking care of n == 0 and bottoms.
--   A generic case treating ordinary data types plus tuples and functions.
--   Type specific extensions for lists and strings.
-- I doubt that it's possible to have a type specific extension that
-- works for all list types, though.
--
-- Anyway, I don't have time improving this right now. All tests are
-- OK, so this should be fine.

gShowsPrec :: Data a => Bool -> Nat -> Prec -> a -> ShowS
gShowsPrec insideList n p (a :: a)
  | n == 0       = showString "_"
  | isBottom a   = showString "_|_"
  | isFunction a = showString "<function>"
  | isTuple a    = showParen True $ drive $
                    List.intersperse (showString ", ") $
                     (continueR (:) [] minPrec a)
  | isString a && isAtom a = when' (not insideList) (showString "\"") $
                             showString "\""  -- End of string.
  | isString a   = when' (not insideList) (showString "\"") $
                    gmapQr (.) id
                     ( id  -- Dummy.
                       `mkQ`
                       (\c -> if n == 1 then showString "_" else
                              if isBottom c then showString "_|_"
                              else showChar c)
                       `extQ`
                       (\(a :: String) -> if n == 1 then id else
                         if isBottom a then showString "_|_"
                         else gShowsPrec True (pred n) minPrec a
                       )
                     ) a
  | isList a && isAtom a = when' (not insideList) (showString "[") $
                           showString "]"  -- End of list.
  | isList a     = when' (not insideList) (showString "[") $
                    gmapQr (.) id
                     ( gShowsPrec False (pred n) minPrec
                      `extQ`
                       (\(a :: a) ->
                         if n == 1 then id
                         else if isBottom a then showString "_|_"
                         else (if not (isAtom a) then showString ", "
                                                 else id) .
                              gShowsPrec True (pred n) minPrec a
                       )
                     ) a
  | isInfix a    = showParen (not (isAtom a) && p > appPrec) $
                       -- We know that we have at least two args,
                       -- because otherwise we would have a function.
                   let (arg1:arg2:rest) =
                          continueR (:) [] (succ appPrec) a
                   in (showParen (not (null rest)) $
                       arg1 .^. showCon a .^. arg2
                      ) . drive rest
  | otherwise    = showParen (not (isAtom a) && p > appPrec) $
                   showCon a .
                   continueL (.^.) nil (succ appPrec) a
                   
    where
    continueL f x p = gmapQl f x (gShowsPrec False (pred n) p)
    continueR f x p = gmapQr f x (gShowsPrec False (pred n) p)

drive          = foldr (.) id
nil            = showString ""
f .^. g        = f . showChar ' ' . g
appPrec        = 10
minPrec        = 0
              -- Remove the surrounding parentheses for infix constructors.
              -- No, don't do that, see the Q test case below. For
              -- lists it would have been correct, though.
showCon a      = showString -- . (if isInfix a then tail . init else id)
                 $ conString $ toConstr a
isAtom a       = glength a == 0
isPrimitive a  = maxConIndex (dataTypeOf a) == 0 
isInfix a      = if isPrimitive a then
                   False
                  else
                   conFixity (toConstr a) == Infix
wrap s         = \s' -> s . s' . s
when' b s      = if b then (s .) else (id .)

------------------------------------------------------------------------
-- Tests

data T = L | B T T deriving (Typeable, Data)

left = B left L

data Q a = Q a ::: a | Q deriving (Typeable, Data)

pr n x template = do
  let s = approxShow n x
  putStr $ show (s == template)
  putStr " |"
  putStr s
  putStrLn "|"

main = do
  pr 4 left "B (B (B (B _ _) L) L) L"
  pr 4 (bottom :: Bool) "_|_"
  pr 4 not "<function>"
  pr 4 ('a','b') "('a', 'b')"
  pr 1 ('a','b') "(_, _)"
  pr 4 (Q ::: 'a' ::: 'b' ::: 'c') "((Q ::: 'a') ::: 'b') ::: 'c'"
  pr 2 (Q ::: 'a' ::: 'b' ::: 'c') "(_ ::: _) ::: 'c'"
  pr 4 "abc" "\"abc\""
  pr 4 [True, False, False] "[True, False, False]"
  pr 2 "abc" "\"a_"
  pr 2 [True, False, False] "[True, _"
  pr 1 "" "\"\""
  pr 1 ([] :: [Bool]) "[]"
  pr 0 "" "_"
  pr 0 ([] :: [Bool]) "_"
  pr 4 ('a' : bottom : bottom) "\"a_|__|_"
  pr 4 ('a' : bottom : bottom : []) "\"a_|__|_\""
  pr 4 [True, bottom] "[True, _|_]"
  pr 4 (True : bottom : bottom) "[True, _|__|_"
  pr 4 (bottom ::: bottom ::: 'b' ::: 'c') "((_|_ ::: _|_) ::: 'b') ::: 'c'"
  pr 2 ('a' : bottom : bottom) "\"a_"
  pr 2 [True, bottom] "[True, _"
  pr 2 (True : bottom : bottom) "[True, _"
  pr 2 (bottom ::: bottom ::: 'b' ::: 'c') "(_ ::: _) ::: 'c'"
