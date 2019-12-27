{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module MyPrelude
  (
    tshow
  , unlessM
  , ByteString
  , LByteString
  , note
  , catMaybes
  , headMay
  , lastMay
  , putStrLn
  , readFile
  , writeFile
  , getLine
  , (#.)
  -- * Monoid map
  , MonoidMap(..)
  -- * Re-exports
  , module X
  )
  where

import           Control.Applicative  as X ((<|>))
import           Control.Monad        as X (filterM, forM, forM_, forever, join,
                                            unless, void, (<=<), (>=>))
import           Control.Monad.Reader as X (MonadReader, ReaderT (..), ask,
                                            asks, runReaderT)
import           Data.Bifunctor       as X (bimap, first, second)
import           Data.Bool            as X (bool)
import           Data.Coerce          as X (Coercible, coerce)
import           Data.Foldable        as X (asum, foldl', for_, traverse_)
import           Data.Int             as X (Int64)
import           Data.List            as X (intercalate, isInfixOf)
import           Data.List.NonEmpty   as X (NonEmpty, head, last, nonEmpty)
import           Data.Map.Strict      as X (Map)
import           Data.Maybe           as X (fromMaybe, isJust, isNothing,
                                            listToMaybe, mapMaybe)
import           Data.Set             as X (Set, member)
import           Data.String          as X (IsString)
import           Data.Text            as X (Text, pack, unpack)
import           Data.Text.Encoding   as X (encodeUtf8)
import           Data.Time            as X (Day (..), UTCTime (..),
                                            defaultTimeLocale, formatTime,
                                            getCurrentTime, parseTimeM)
import           GHC.Generics         as X (Generic)
import           Numeric.Natural      as X
import           Prelude              as X hiding (getLine, head, last, lookup,
                                            putStrLn, readFile, writeFile)
import           UnliftIO             as X hiding (Handler)

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Foldable        as F
import qualified Data.Text.IO         as T

import qualified Data.Map.Strict      as M

import           GHC.Exts             (IsList (..))

-- | A strict bytestring
type ByteString = B.ByteString
-- | A lazy bytestring
type LByteString = BL.ByteString

-- | Like 'show' but with 'Text'
tshow :: Show a => a -> Text
tshow = pack . show

-- | A monadic 'unless'
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM predicate act = predicate >>= \p -> unless p act

-- | Lift a 'Maybe a' into 'Either e a'
note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

-- | Catenate a traversable of maybes into a list
catMaybes :: Traversable t => t (Maybe a) -> [a]
catMaybes = concatMap F.toList

-- | 'putStrLn' with 'Text'
putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . T.putStrLn

-- | Read a file into a strict 'ByteString'
--
-- Prefer conduits or pipes over this
readFile :: MonadIO m => FilePath -> m ByteString
readFile = liftIO . B.readFile

-- | Write a file from a strict 'ByteString'
--
-- Prefer conduits or pipes over this
writeFile :: MonadIO m => FilePath -> ByteString -> m ()
writeFile path = liftIO . B.writeFile path

-- | Get a line from stdin
getLine :: MonadIO m => m Text
getLine = liftIO T.getLine

-- | Get the first element of a foldable
headMay :: Foldable f => f a -> Maybe a
headMay = fmap head . nonEmpty . F.toList

-- | Get the last element of a foldable
--
-- Note that this is partial in the sense that i's a bottom in case of an infinite structure
lastMay :: Foldable f => f a -> Maybe a
lastMay = fmap last . nonEmpty . F.toList

-- | Coerce a value
--
-- See 'microlens' and 'profunctors'
( #. ) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
( #. ) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b


-- | Monoid map
-- A containers 'Map' but with semigroup value


newtype MonoidMap k v = MonoidMap { unMonoidMap :: Map k v }

instance (Ord k, Semigroup v) => Semigroup (MonoidMap k v) where
  MonoidMap m1 <> MonoidMap m2 = MonoidMap $ M.unionWith (<>) m1 m2

instance (Ord k, Semigroup v) => Monoid (MonoidMap k v) where
  mempty = MonoidMap mempty

instance Ord k => IsList (MonoidMap k v) where
  type Item (MonoidMap k v) = (k, v)
  fromList = MonoidMap . M.fromList
  toList = M.toList . unMonoidMap
