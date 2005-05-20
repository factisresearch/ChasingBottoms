-- |
-- Module      :  ChasingBottoms.IsType
-- Copyright   :  (c) Nils Anders Danielsson 2004
-- License     :  See the file LICENSE.
-- 
-- Maintainer  :  http://www.cs.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (GHC-specific)
--
-- Internal helper functions.

module ChasingBottoms.IsType( isFunction, isTuple, isList, isString ) where

import Data.Typeable

-- | '@isFunction@ f' returns 'True' iff the top level \"constructor\"
-- of @f@ is a function arrow.
isFunction :: Typeable a => a -> Bool
isFunction f = con f == con not  -- TyCon is abstract.

con :: Typeable a => a -> TyCon
con = typerepTyCon . typeOf

-- | This function is rather fragile, but should be OK. It is only
-- used by "ChasingBottoms.ApproxShow", which should only be used for
-- debugging purposes anyway. The unit type is not considered to be a
-- tuple.
isTuple :: Typeable a => a -> Bool
isTuple x = if null s then False else head s == ','
  where s = tyconString (con x)

isString :: Typeable a => a -> Bool
isString x = isList x && typerepArgs (typeOf x) == typerepArgs (typeOf "")

isList :: Typeable a => a -> Bool
isList x = con x == con ""
