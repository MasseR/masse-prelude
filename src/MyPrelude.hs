module MyPrelude
  ( module X
  , tshow
  , unlessM
  , ByteString
  , LByteString
  , note
  , catMaybes
  , headMay
  , lastMay
  , putStrLn
  , readFile
  , getLine
  )
  where

import           Control.Applicative  as X ((<|>))
import           Control.Monad        as X (filterM, forM, forM_, forever, join,
                                            unless, void, (<=<), (>=>))
import           Control.Monad.Reader as X (MonadReader, ReaderT (..), ask,
                                            asks, runReaderT)
import           Data.Bifunctor       as X (bimap, first, second)
import           Data.Bool            as X (bool)
import           Data.Foldable        as X (asum, foldl', for_, traverse_)
import           Data.Int             as X (Int64)
import           Data.List            as X (intercalate, isInfixOf)
import           Data.List.NonEmpty   (NonEmpty, head, last, nonEmpty)
import           Data.Map.Strict      as X (Map)
import           Data.Maybe           as X (fromMaybe, isJust, isNothing,
                                            listToMaybe, mapMaybe)
import           Data.Set             as X (Set, member)
import           Data.Text            as X (Text, pack, unpack)
import           Data.Text.Encoding   as X (encodeUtf8)
import           Data.Time            as X (Day (..), UTCTime (..),
                                            defaultTimeLocale, formatTime,
                                            getCurrentTime)
import           GHC.Generics         as X (Generic)
import           Prelude              as X hiding (getLine, head, last, lookup,
                                            putStrLn, readFile)
import           UnliftIO             as X hiding (Handler)

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Foldable        as F
import qualified Data.Text.IO         as T

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

-- | Read a file into a strict bytestring
--
-- Prefer conduits or pipes over this
readFile :: MonadIO m => FilePath -> m ByteString
readFile = liftIO . B.readFile

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
