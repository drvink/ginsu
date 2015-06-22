{-# LANGUAGE CPP, NoImplicitPrelude, TypeOperators, ExistentialQuantification, PolymorphicComponents, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ReadP.ByteString
-- Copyright   :  (c) The University of Glasgow 2002
--             :  (c) Gracjan Polak 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  gracjanpolak@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (local universal quantification)
--
-- This is a library of parser combinators, originally written by Koen Claessen.
-- It parses all alternatives in parallel, so it never keeps hold of
-- the beginning of the input string, a common source of space leaks with
-- other parsers.  The '('+++')' choice combinator is genuinely commutative;
-- it makes no difference which branch is \"shorter\".
--
-- Adapted to use 'Data.ByteString' by Gracjan Polak. Designed as a drop-in
-- replacement for 'Text.ParserCombinators.ReadP'.
--
-- minor modifications by John Meacham for use in ginsu
--
-----------------------------------------------------------------------------

module Text.ParserCombinators.ReadP.ByteString
  (
  -- * The 'ReadP' type
  ReadP,      -- :: * -> *; instance Functor, Monad, MonadPlus

  -- * Primitive operations
  skip,       -- :: Int -> ReadP Word8
  look,       -- :: ReadP ByteString
  (+++),      -- :: ReadP a -> ReadP a -> ReadP a
  (<++),      -- :: ReadP a -> ReadP a -> ReadP a
  countsym,   -- :: ReadP a -> ReadP (Int, a)

  -- * Other operations
  get,        -- :: ReadP Word8
  pfail,      -- :: ReadP a
  satisfy,    -- :: (Word8 -> Bool) -> ReadP Word8
  char,       -- :: Word8 -> ReadP Word8
  string,     -- :: ByteString -> ReadP ByteString
  gather,     -- :: ReadP a -> ReadP (ByteString, a)
  munch,      -- :: (Word8 -> Bool) -> ReadP ByteString
  munch1,     -- :: (Word8 -> Bool) -> ReadP ByteString
  skipSpaces, -- :: ReadP ()
  choice,     -- :: [ReadP a] -> ReadP a
  count,      -- :: Int -> ReadP a -> ReadP [a]
  between,    -- :: ReadP open -> ReadP close -> ReadP a -> ReadP a
  option,     -- :: a -> ReadP a -> ReadP a
  optional,   -- :: ReadP a -> ReadP ()
  many,       -- :: ReadP a -> ReadP [a]
  many1,      -- :: ReadP a -> ReadP [a]
  skipMany,   -- :: ReadP a -> ReadP ()
  skipMany1,  -- :: ReadP a -> ReadP ()
  sepBy,      -- :: ReadP a -> ReadP sep -> ReadP [a]
  sepBy1,     -- :: ReadP a -> ReadP sep -> ReadP [a]
  endBy,      -- :: ReadP a -> ReadP sep -> ReadP [a]
  endBy1,     -- :: ReadP a -> ReadP sep -> ReadP [a]
  chainr,     -- :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
  chainl,     -- :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
  chainl1,    -- :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
  chainr1,    -- :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
  manyTill,   -- :: ReadP a -> ReadP end -> ReadP [a]

  -- * Running a parser
  ReadS,      -- :: *; = ByteString -> [(a,ByteString)]
  readP_to_S, -- :: ReadP a -> ReadS a
  readS_to_P, -- :: ReadS a -> ReadP a
  )
 where

import GHC.Base (Alternative(empty, (<|>)), ap)
import Control.Monad ( MonadPlus(..), liftM2, Monad, (>>), (>>=),
                       return, fail, Functor, fmap, replicateM, void )
import Prelude ( (+), (-), (++), Int, Bool(..), (==), error,
                 (.), (>=), compare, Ordering(..), const,
                 Applicative(pure, (<*>)) )
import Data.Word (Word8)
import Data.ByteString (ByteString,length,take,takeWhile,null)

#ifdef BYTESTRING_BASE_ONLY
import Data.ByteString.Base (unsafeDrop,unsafeHead,isSpaceWord8)
#else
import Data.ByteString.Unsafe (unsafeTake,unsafeDrop,unsafeHead)
import Data.ByteString.Internal (isSpaceWord8)
#endif

infixr 5 +++, <++

------------------------------------------------------------------------
-- ReadS

-- | A parser for a type @a@, represented as a function that takes a
-- 'ByteString' and returns a list of possible parses as @(a,'ByteString')@ pairs.
--
-- Note that this kind of backtracking parser is very inefficient;
-- reading a large structure may be quite slow (cf 'ReadP').
type ReadS a = ByteString -> [(a,ByteString)]

-- ---------------------------------------------------------------------------
-- The P type
-- is representation type -- should be kept abstract

data P a
  = Skip {-# UNPACK #-} !Int (P a)
  | Look (ByteString -> P a)
  | Fail
  | Result a (P a)
  | Final [(a,ByteString)] -- invariant: list is non-empty!
  deriving Functor

-- Applicative

instance Applicative P where
  pure = return
  (<*>) = ap

-- Monad, MonadPlus

instance Monad (P) where
  return x = Result x Fail

  (Skip n f)   >>= k = Skip n (f >>= k)
  (Look f)     >>= k = Look (\s -> f s >>= k)
  Fail         >>= _ = Fail
  (Result x p) >>= k = k x `mplus` (p >>= k)
  (Final r)    >>= k = final [ys' | (x,s) <- r, ys' <- run (k x) s]

  fail _ = Fail

instance Alternative P where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (P) where
  mzero = Fail

  -- most common case: two skips are combined
  Skip n1 f1 `mplus` Skip n2 f2 =
      case compare n1 n2 of
          LT -> Skip n1 (f1 `mplus` Skip (n2-n1) f2)
          EQ -> Skip n1 (f1 `mplus` f2)
          GT -> Skip n2 (Skip (n1-n2) f1 `mplus` f2)

  -- results are delivered as soon as possible
  Result x p `mplus` q          = Result x (p `mplus` q)
  p          `mplus` Result x q = Result x (p `mplus` q)

  -- fail disappears
  Fail       `mplus` p          = p
  p          `mplus` Fail       = p

  -- two finals are combined
  -- final + look becomes one look and one final (=optimization)
  -- final + sthg else becomes one look and one final
  Final r    `mplus` Final t    = Final (r ++ t)
  Final r    `mplus` Look f     = Look (\s -> Final (r ++ run (f s) s))
  Final r    `mplus` p          = Look (\s -> Final (r ++ run p s))
  Look f     `mplus` Final r    = Look (\s -> Final (run (f s) s ++ r))
  p          `mplus` Final r    = Look (\s -> Final (run p s ++ r))

  -- two looks are combined (=optimization)
  -- look + sthg else floats upwards
  Look f     `mplus` Look g     = Look (\s -> f s `mplus` g s)
  Look f     `mplus` p          = Look (\s -> f s `mplus` p)
  p          `mplus` Look f     = Look (\s -> p `mplus` f s)

-- ---------------------------------------------------------------------------
-- The ReadP type

newtype ReadP a = R (forall b . (a -> P b) -> P b)

-- Functor, Monad, MonadPlus

instance Functor (ReadP) where
  fmap h (R f) = R (\k -> f (k . h))

instance Applicative (ReadP) where
  pure = return
  (<*>) = ap

instance Monad (ReadP) where
  return x  = R (\k -> k x)
  fail _    = R (const Fail)
  R m >>= f = R (\k -> m (\a -> let R m' = f a in m' k))

instance Alternative ReadP where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (ReadP) where
  mzero = pfail
  mplus = (+++)

-- ---------------------------------------------------------------------------
-- Operations over P

final :: [(a,ByteString)] -> P a
-- Maintains invariant for Final constructor
final [] = Fail
final r  = Final r

run :: P a -> ReadS a
run (Skip n f)  cs | length cs >=n =
         run f (unsafeDrop n cs)
run (Look f)     s     = run (f s) s
run (Result x p) s     = (x,s) : run p s
run (Final r)    _     = r
run _            _     = []

-- ---------------------------------------------------------------------------
-- Operations over ReadP


skip :: Int -> ReadP ()
skip 0 = R (\f -> f ())
skip n = R (\f -> Skip n (f ()))

get :: ReadP Word8
-- ^ Consumes and returns the next character.
--   Fails if there is no input left.
get = do
    s <- look
    skip 1
    return (unsafeHead s)

look :: ReadP ByteString
-- ^ Look-ahead: returns the part of the input that is left, without
--   consuming it.
look = R Look

pfail :: ReadP a
-- ^ Always fails.
pfail = R (const Fail)

(+++) :: ReadP a -> ReadP a -> ReadP a
-- ^ Symmetric choice.
R f1 +++ R f2 = R (\k -> f1 k `mplus` f2 k)

(<++) :: ReadP a -> ReadP a -> ReadP a
-- ^ Local, exclusive, left-biased choice: If left parser
--   locally produces any result at all, then right parser is
--   not used.
R f <++ q =
  do s <- look
     probe (f return) s 0
 where
  probe (Skip m f)     cs    n  | length cs >= m = probe f (unsafeDrop m cs) (n+m)
  probe (Look f)       s     n = probe (f s) s n
  probe p@(Result _ _) _     n = skip n >> R (p >>=)
  probe (Final r)      _     _ = R (Final r >>=)
  probe _              _     _ = q

gather :: ReadP a -> ReadP (ByteString, a)
-- ^ Transforms a parser into one that does the same, but
--   in addition returns the exact characters read.
--   IMPORTANT NOTE: 'gather' gives a runtime error if its first argument
--   is built using any occurrences of readS_to_P.
gather p = do
    s <- look
    (l,r) <- countsym p
    return (unsafeTake l s,r)
--    return (unsafeDrop l s,r)

countsym :: ReadP a -> ReadP (Int, a)
-- ^ Transforms a parser into one that does the same, but
--   in addition returns the exact number of characters read.
--   IMPORTANT NOTE: 'countsym' gives a runtime error if its first argument
--   is built using any occurrences of readS_to_P.
countsym (R m) =
  R (\k -> gath 0 (m (\a -> return (\s -> k (s,a)))))
 where
  gath 0 _   | False  = Fail
  gath l (Skip n f)   = Skip n (gath (l+n) f)
  gath _ Fail         = Fail
  gath l (Look f)     = Look (gath l . f)
  gath l (Result k p) = k l `mplus` gath l p
  gath _ (Final _)    = error "do not use readS_to_P in gather or countsym!"

-- ---------------------------------------------------------------------------
-- Derived operations

satisfy :: (Word8 -> Bool) -> ReadP Word8
-- ^ Consumes and returns the next character, if it satisfies the
--   specified predicate.
satisfy p = do
    c <- get
    if p c
        then return c
        else pfail

char :: Word8 -> ReadP Word8
-- ^ Parses and returns the specified character.
char c = satisfy (c ==)

string :: ByteString -> ReadP ByteString
-- ^ Parses and returns the specified string.
string this = do
    s <- look
    let l = length this
    let w = take l s
    if this == w
        then skip (length this) >> return this
        else pfail

munch :: (Word8 -> Bool) -> ReadP ByteString
-- ^ Parses the first zero or more characters satisfying the predicate.
munch p =
  do s <- look
     let k = takeWhile p s
     skip (length k)
     return k

munch1 :: (Word8 -> Bool) -> ReadP ByteString
-- ^ Parses the first one or more characters satisfying the predicate.
munch1 p =
  do s <- look
     let k = takeWhile p s
     if null k
         then pfail
         else skip (length k) >> return k

choice :: [ReadP a] -> ReadP a
-- ^ Combines all parsers in the specified list.
choice []     = pfail
choice [p]    = p
choice (p:ps) = p +++ choice ps

skipSpaces :: ReadP ()
-- ^ Skips all whitespace.
skipSpaces = void (munch isSpaceWord8)

count :: Int -> ReadP a -> ReadP [a]
-- ^ @count n p@ parses @n@ occurrences of @p@ in sequence. A list of
--   results is returned.
count = replicateM

between :: ReadP open -> ReadP close -> ReadP a -> ReadP a
-- ^ @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between open close p = do open
                          x <- p
                          close
                          return x

option :: a -> ReadP a -> ReadP a
-- ^ @option x p@ will either parse @p@ or return @x@ without consuming
--   any input.
option x p = p +++ return x

optional :: ReadP a -> ReadP ()
-- ^ @optional p@ optionally parses @p@ and always returns @()@.
optional p = void p +++ return ()

many :: ReadP a -> ReadP [a]
-- ^ Parses zero or more occurrences of the given parser.
many p = return [] +++ many1 p

many1 :: ReadP a -> ReadP [a]
-- ^ Parses one or more occurrences of the given parser.
many1 p = liftM2 (:) p (many p)

skipMany :: ReadP a -> ReadP ()
-- ^ Like 'many', but discards the result.
skipMany p = void (many p)

skipMany1 :: ReadP a -> ReadP ()
-- ^ Like 'many1', but discards the result.
skipMany1 p = p >> skipMany p

sepBy :: ReadP a -> ReadP sep -> ReadP [a]
-- ^ @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy p sep = sepBy1 p sep +++ return []

sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
-- ^ @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

endByDo :: ReadP a -> ReadP sep -> ReadP a
endByDo p sep = do x <- p ; sep ; return x

endBy :: ReadP a -> ReadP sep -> ReadP [a]
-- ^ @endBy p sep@ parses zero or more occurrences of @p@, separated and ended
--   by @sep@.
endBy p sep = many (endByDo p sep)

endBy1 :: ReadP a -> ReadP sep -> ReadP [a]
-- ^ @endBy p sep@ parses one or more occurrences of @p@, separated and ended
--   by @sep@.
endBy1 p sep = many1 (endByDo p sep)

chainr :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
-- ^ @chainr p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /right/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainr p op x = chainr1 p op +++ return x

chainl :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
-- ^ @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl p op x = chainl1 p op +++ return x

chainr1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
-- ^ Like 'chainr', but parses one or more occurrences of @p@.
chainr1 p op = scan
  where scan   = p >>= rest
        rest x = do f <- op
                    y <- scan
                    return (f x y)
                 +++ return x

chainl1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
-- ^ Like 'chainl', but parses one or more occurrences of @p@.
chainl1 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 +++ return x

manyTill :: ReadP a -> ReadP end -> ReadP [a]
-- ^ @manyTill p end@ parses zero or more occurrences of @p@, until @end@
--   succeeds. Returns a list of values returned by @p@.
manyTill p end = scan
  where scan = (end >> return []) <++ liftM2 (:) p scan

-- ---------------------------------------------------------------------------
-- Converting between ReadP and Read

readP_to_S :: ReadP a -> ReadS a
-- ^ Converts a parser into a Haskell ReadS-style function.
--   This is the main way in which you can \"run\" a 'ReadP' parser:
--   the expanded type is
-- @ readP_to_S :: ReadP a -> ByteString -> [(a,ByteString)] @
readP_to_S (R f) = run (f return)

readS_to_P :: ReadS a -> ReadP a
-- ^ Converts a Haskell ReadS-style function into a parser.
--   Warning: This introduces local backtracking in the resulting
--   parser, and therefore a possible inefficiency.
readS_to_P r =
  R (\k -> Look (\s -> final [bs'' | (a,s') <- r s, bs'' <- run (k a) s']))

