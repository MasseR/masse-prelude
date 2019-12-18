module MyPrelude
  ( module X
  , tshow
  , unlessM
  , ByteString
  , LByteString
  , note
  )
  where

import           Control.Applicative  as X ((<|>))
import           Control.Monad        as X (filterM, forM, forM_, forever, join,
                                            unless, void, (<=<))
import           Control.Monad.Reader as X (MonadReader, ReaderT (..), ask,
                                            asks, runReaderT)
import           Data.Bifunctor       as X (bimap, first, second)
import           Data.Bool            as X (bool)
import           Data.Foldable        as X (foldl', for_, traverse_)
import           Data.Int             as X (Int64)
import           Data.List            as X (intercalate, isInfixOf)
import           Data.Map.Strict      as X (Map)
import           Data.Maybe           as X (isJust, isNothing)
import           Data.Set             as X (Set, member)
import           Data.Text            as X (Text, pack, unpack)
import           Data.Text.Encoding   as X (encodeUtf8)
import           Data.Time            as X (Day, UTCTime (..),
                                            defaultTimeLocale, formatTime,
                                            getCurrentTime)
import           GHC.Generics         as X (Generic)
import           Prelude              as X hiding (lookup, putStrLn, readFile)
import           UnliftIO             as X hiding (Handler)

import           Data.ByteString      as X (readFile)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Foldable        as F
import qualified Data.Text.IO         as T

type ByteString = B.ByteString
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

catMaybes :: Traversable t => t (Maybe a) -> [a]
catMaybes = concatMap F.toList

-- | 'putStrLn' with 'Text'
putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . T.putStrLn

-- | Read a file into a lazy bytestring
--
-- Prefer conduits or pipes over this
readFile :: MonadIO m => FilePath -> m LByteString
readFile = liftIO . BL.readFile
