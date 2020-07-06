{-# LANGUAGE NoImplicitPrelude #-}
{- |
Name: Cronus

This module is the big hammer of alternatives to the standard Prelude.
It is meant to optimize compatibility with the standard Prelude and the
various packages which are built on top of it, while exposing types and
functions that I want available to me in the vast majority of "real world
applications."
-}
module Cronus
( 
--  ** Bool
  Bool(True, False)
, (&&), (||), not, otherwise
-- ** Maybe
, Maybe(Just, Nothing)
, maybe
, catMaybes
, mapMaybe
, listToMaybe
, maybeToList
, fromMaybe
, isNothing
, isJust
-- ** Eq
, Eq(..)
-- ** Ord
, Ordering(LT, EQ, GT)
, Ord(..)
-- ** Char
, Char
, isControl
, isSpace
, isLower
, isUpper
, isAlpha
, isAlphaNum
, isPrint
, isDigit
, isOctDigit
, isHexDigit
, isLetter
, isMark
, isNumber
, isPunctuation
, isSymbol
, isSeparator
, isAscii
, isLatin1
, isAsciiUpper
, isAsciiLower
, ord
, chr
-- ** Convenience Functions
, fst, snd
, curry
, uncurry
, on
, flip
, const
, until
, (&)
, ($)
-- ** Enum
, Enum(..)
-- ** Bounded
, Bounded(..)
-- ** Numeric Types
, Int
, Int8, Int16, Int32, Int64
, Integer
, Natural
, Float
, Double
, Word
, Word8, Word16, Word32, Word64
-- ** The Numeric Hierarchy
, Rational
, Num(..), subtract
, Real(toRational)
, Integral(..)
, Fractional(..)
, Floating(..)
, RealFrac(..)
, RealFloat(..)
-- ** Functions on Numbers
, even
, odd
, gcd
, lcm
, (^)
, (^^)
, fromIntegral
, realToFrac
-- ** Show
, Show(..)
-- ** Read
, Read(..)
, readMaybe
-- ** Semigroup
, Semigroup(..)
-- ** Monoid
, Monoid(..)
-- ** Functor
, Functor(..)
, (<$>)
, void
-- ** Applicative
, Applicative(..)
, Alternative(..)
, ZipList(..)
, (<**>)
, liftA
, liftA3
, optional
-- ** Monad
, Monad(..)
, mfilter
, mapAndUnzipM
, zipWithM
, zipWithM_
, foldM
, foldM_
, replicateM
, replicateM_
, guard
, when
, unless
, liftM
, liftM2
, liftM3
, liftM4
, liftM5
, ap 
, (<$!>)
, MonadPlus(..)
, MonadFail(..)
, mapM_
, sequence_
, (=<<)
, Kleisli(..)
-- ** Foldable
, Foldable(..)
, foldrM
, foldlM
, for_
, for
, maximumBy
, minimumBy
, find
, msum
-- ** Traversable
, Traversable(..)
, traverse_
, sequenceA_
, asum
, forM_
, forM
, mapAccumL
, mapAccumR
-- ** Category
, Category(..)
, (>>>)
, (<<<)
-- ** error and undefined
, error
, undefined
-- ** Strictness
, seq
, ($!)
, deepseq
, ($!!)
, force
, NFData(..)
-- ** List Functions
, reverse
, and
, or
, any
, all
, concat
, concatMap
, scanl
, scanr
, iterate
, repeat
, replicate
, cycle
, take
, drop
, takeWhile
, dropWhile
, span
, break
, splitAt
, notElem
, lookup
, zip
, zip3
, zipWith
, zipWith3
, unzip
, unzip3
-- ** IO
, IO
-- ** ST
, ST
, runST
, STRef
, newSTRef
, writeSTRef
, modifySTRef
, modifySTRef'
-- ** Exceptions
, throwIO
, SomeException
, Exception(..)
, catch
, ArithException(..)
, ArrayException(..)
, AssertionFailed(..)
, SomeAsyncException(..)
, AsyncException(..)
, asyncExceptionToException
, asyncExceptionFromException
, NonTermination(..)
, NestedAtomically(..)
, BlockedIndefinitelyOnMVar(..)
, BlockedIndefinitelyOnSTM(..)
, AllocationLimitExceeded(..)
, CompactionFailed(..)
, Deadlock(..)
, NoMethodError(..)
, PatternMatchFail(..)
, RecConError(..)
, RecSelError(..)
, RecUpdError(..)
, ErrorCall(..)
, TypeError(..)
, throw
, ioError
, IOError
, catches
, Handler(..)
, catchJust
, handle
, handleJust
, try
, tryJust
, evaluate
, mapException
, mask
, mask_
, uninterruptibleMask
, uninterruptibleMask_
, MaskingState(..)
, getMaskingState
, interruptible
, allowInterrupt
, assert
, bracket
, bracket_
, bracketOnError
, finally
, onException
-- ** Typeable
, Typeable(..)
, typeOf
, typeRep
, TypeRep
, (:~:)(Refl)
, (:~~:)(HRefl)
, cast
, eqT
, gcast
, gcast1
, gcast2
, typeRepFingerprint
-- ** Proxy
, Proxy(Proxy)
-- ** Concurrency
, forkIO
, ThreadId
, myThreadId
, forkFinally
, forkIOWithUnmask
, killThread
, throwTo
, forkOn
, forkOnWithUnmask
, getNumCapabilities
, setNumCapabilities
, threadCapability
, yield
, threadDelay
, threadWaitRead
, threadWaitWrite
, threadWaitReadSTM
, MVar
, newMVar
, takeMVar
, readMVar
, swapMVar
, tryTakeMVar
, putMVar
, tryPutMVar
, withMVar
, isEmptyMVar
, modifyMVar_
, modifyMVar
, modifyMVarMasked
, tryReadMVar
, mkWeakMVar
, Chan
, newChan
, writeChan
, readChan
, dupChan
, getChanContents
, writeList2Chan
, QSem
, newQSem
, waitQSem
, signalQSem
, QSemN
, newQSemN
, waitQSemN
, signalQSemN
, rtsSupportsBoundThreads
, forkOSWithUnmask
, isCurrentThreadBound
, runInBoundThread
, runInUnboundThread
, mkWeakThreadId
-- ** ByteString
, ByteString
-- ** Text
, Text
-- ** Ordered Containers
, Graph
, IntMap
, Map
, Set
, Seq
, IntSet
, Tree
-- ** Unordered Containers
, Hashable(..)
, HashMap
, HashSet
-- ** MonadTrans
, MonadTrans(..)
-- ** MonadIO
, MonadIO(..)
-- ** Reader
, ReaderT(..)
, MonadReader(..)
, asks
, Reader
, runReader
, mapReader
, withReader
-- ** State
, StateT(..)
, MonadState(..)
, State
, modify
, modify'
, gets
-- ** CPSed Writer
, WriterT(..)
, MonadWriter(..)
, listens
, censor
, Writer
, runWriter
, execWriter
, mapWriter
, execWriterT
, mapWriterT
-- ** MaybeT
, MaybeT(..)
, mapMaybeT
, maybeToExceptT
, exceptToMaybeT
-- ** ExceptT
, ExceptT(..)
, Except
, except
, runExcept
, mapExcept
, withExcept
, mapExceptT
, withExceptT
, throwE
, catchE
-- ** RWST with CPSed Writer
, RWST(..)
, RWS
, rws
, runRWS
, evalRWS
, execRWS
, mapRWS
, withRWS
, evalRWST
, execRWST
, mapRWST
, withRWST
-- ** Functor Combinators
, Identity(..)
, Const(..)
, Sum(..)
, Product(..)
, Compose(..)
-- ** Contravariant Functor
, Contravariant(..)
-- ** Unique
, Unique(..)
, newUnique
, hashUnique
-- ** Version
, Version(..)
-- ** Void
, Void(..)
-- ** Bitraversable
, Bitraversable(..)
, bisequenceA
, bisequence
, bimapM
, bifor
, biforM
, bimapAccumL
, bimapAccumR
, bimapDefault
, bifoldMapDefault
-- ** Data.Coerce
, Coercible(..)
, coerce
-- ** Dynamic
, Dynamic
, toDyn
, fromDyn
, fromDynamic
, dynApply
, dynApp
, dynTypeRep
-- ** Either
, Either(Left, Right)
, either
, lefts
, rights
, isLeft
, isRight
, fromLeft
, fromRight
, partitionEithers
-- ** Ix
, Ix(..)
-- ** IORef
, IORef
, newIORef
, readIORef
, writeIORef
, modifyIORef
, modifyIORef'
, atomicModifyIORef
, atomicModifyIORef'
, atomicWriteIORef
, mkWeakIORef
-- ** MonadFix
, MonadFix(..)
, fix
-- ** Arrow Operators
, (&&&)
, (***)
-- ** Bifunctor
, Bifunctor(..)
-- ** Profunctor
, Profunctor(..)
, Strong(..)
, Choice(..)
, Closed(..)
, curry'
, Mapping(..)
, Costrong(..)
, Cochoice(..)
, Star(..)
, Costar(..)
, Forget(..)
-- ** Unboxed Vectors
, Vector
, MVector
, Unbox(..)
-- ** Comonads
, Comonad(..)
, liftW
, wfix
, cfix
, kfix
, (=>=)
, (=<=)
, (<<=)
, (=>>)
, ComonadApply(..)
, (<@@>)
, liftW2
, liftW3
, Cokleisli(..)
-- ** Fixed
, Fixed(..)
, HasResolution(..)
, Deci
, Centi
, Milli
, Micro
, Nano
, Pico
-- ** Time
, UTCTime(..)
, UniversalTime(..)
, DiffTime(..)
, secondsToDiffTime
, picosecondsToDiffTime
, diffTimeToPicoseconds
, NominalDiffTime(..)
, secondsToNominalDiffTime
, nominalDiffTimeToSeconds
, nominalDay
, addUTCTime
, diffUTCTime
, getCurrentTime
, getTime_resolution
) where

import Data.Profunctor
import Data.Bifunctor (Bifunctor(..))
import Control.Arrow ((&&&), (***), Kleisli(..))
import Data.Function ((&), ($), on, flip, const)
import Control.Category
import GHC.Num
import Prelude hiding ((.), id)
import Data.Int
import Control.Monad
import Control.Applicative
import Data.Word
import Data.Foldable
import Data.Traversable
import Text.Read
import Control.Monad.ST
import Control.Concurrent
import Control.Exception
import Data.Typeable
import Control.DeepSeq
import Text.Read (readMaybe)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.IntMap (IntMap)
import Data.IntMap.Lazy (IntMap)
import Data.Graph (Graph)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.IntSet (IntSet)
import Data.Tree (Tree)
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS.CPS hiding (reader, ask, local, asks, writer, tell, listen, listens, pass, censor, state, get, put, modify, gets)
import Control.Monad.Trans.Except
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Unique
import Data.Version
import Data.Bitraversable
import Data.Void
import Data.Coerce (Coercible, coerce)
import Data.Dynamic
import Data.Maybe
import Data.Either
import Data.Ix
import Data.IORef
import Data.STRef
import Data.Char
import Data.Vector.Unboxed (Vector, MVector, Unbox(..))
import Control.Comonad
import Data.Time.Clock
import Data.Fixed
