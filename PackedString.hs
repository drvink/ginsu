{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PackedString
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- An efficient implementation of strings.
--
-----------------------------------------------------------------------------


-- Original GHC implementation by Bryan O\'Sullivan,
-- rewritten to use UArray by Simon Marlow.
-- modified by John Meacham for use in ginsu
-- changed to a trivial wrapper for Data.ByteString.UTF8 by Dylan Simon

module PackedString (
	-- * The @PackedString@ type
        PackedString,      -- abstract, instances: Eq, Ord, Show, Typeable

         -- * Converting to and from @PackedString@s
	packString,  -- :: String -> PackedString
	unpackPS,    -- :: PackedString -> String
        showsPS,
        -- toString,
--        lengthPS,
--        utfLengthPS,

	joinPS,      -- :: PackedString -> [PackedString] -> PackedString
	-- * List-like manipulation functions
	nilPS,       -- :: PackedString
	consPS,      -- :: Char -> PackedString -> PackedString
	nullPS,      -- :: PackedString -> Bool
	appendPS,    -- :: PackedString -> PackedString -> PackedString
--        foldrPS,
        hashPS,
--        filterPS,
--        foldlPS,
--        headPS,
	concatPS    -- :: [PackedString] -> PackedString



    ) where

import Data.Typeable
import Data.Int
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as U
import qualified Data.String.UTF8 as US
import Data.Bits
import GHC.Generics (Generic)
import Data.Hashable
import Data.Monoid

instance Monoid PackedString where
    mempty = nilPS
    mappend x y = appendPS x y
    mconcat xs = concatPS xs

-- -----------------------------------------------------------------------------
-- PackedString type declaration

-- | A space-efficient representation of a 'String', which supports various
-- efficient operations.  A 'PackedString' contains full Unicode 'Char's.
-- much like UTF8 ByteString
newtype PackedString = PS { unPS :: U.ByteString }
    deriving(Typeable,Binary,Eq,Ord,Generic)


instance Show PackedString where
    showsPrec p (PS ps) = showsPrec p (US.fromRep ps)

instance Hashable PackedString

-- -----------------------------------------------------------------------------
-- Constructor functions

-- | The 'nilPS' value is the empty string.
nilPS :: PackedString
nilPS = PS BS.empty

-- | The 'consPS' function prepends the given character to the
-- given string.
consPS :: Char -> PackedString -> PackedString
consPS c (PS cs) = PS $ BS.append (U.fromString [c]) cs

-- | Convert a 'String' into a 'PackedString'
packString :: String -> PackedString
packString = PS . U.fromString


-- -----------------------------------------------------------------------------
-- Destructor functions (taking PackedStrings apart)


unpackPS :: PackedString -> String
unpackPS = U.toString . unPS

showsPS :: PackedString -> String -> String
showsPS ps = (unpackPS ps ++)


-- | The 'nullPS' function returns True iff the argument is null.
nullPS :: PackedString -> Bool
nullPS = BS.null . unPS

-- | The 'appendPS' function appends the second string onto the first.
appendPS :: PackedString -> PackedString -> PackedString
appendPS (PS xs) (PS ys) = PS $ BS.append xs ys


hashPS :: PackedString -> Int32
hashPS (PS arr) = f 5381 (BS.unpack arr) where
    f x [] = x
    f m (c:cs) = n `seq` f n cs where
        n = ((m `shiftL` 5) + m ) `xor` fromIntegral c

-- | The 'concatPS' function concatenates a list of 'PackedString's.
concatPS :: [PackedString] -> PackedString
concatPS = PS . BS.concat . map unPS

------------------------------------------------------------

-- | The 'joinPS' function takes a 'PackedString' and a list of 'PackedString's
-- and concatenates the list after interspersing the first argument between
-- each element of the list.
joinPS :: PackedString -> [PackedString] -> PackedString
joinPS filler pss = concatPS (splice pss)
 where
  splice []  = []
  splice [x] = [x]
  splice (x:y:xs) = x:filler:splice (y:xs)

-- ToDo: the obvious generalisation
{-
  Some properties that hold:

  * splitPS x ls = ls'
      where False = any (map (x `elemPS`) ls')

  * joinPS (packString [x]) (splitPS x ls) = ls
-}

