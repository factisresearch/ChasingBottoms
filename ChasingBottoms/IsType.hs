-- | Internal helper functions.

module ChasingBottoms.IsType( isFunction, isTuple, isList, isString ) where

import Data.Typeable

-- isFunction f returns True iff the top level "constructor" of f is a
-- function arrow.
isFunction :: Typeable a => a -> Bool
isFunction f = con f == con not  -- TyCon is abstract.

con :: Typeable a => a -> TyCon
con = typerepTyCon . typeOf

-- This function is rather fragile, but should be OK.
-- The unit type is not considered to be a tuple.
isTuple :: Typeable a => a -> Bool
isTuple x = if null s then False else head s == ','
  where s = tyconString (con x)

isString :: Typeable a => a -> Bool
isString x = isList x && typerepArgs (typeOf x) == typerepArgs (typeOf "")

isList :: Typeable a => a -> Bool
isList x = con x == con ""

------------------------------------------------------------------------
-- Tests

tests =
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

testsOK = and tests
