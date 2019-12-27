{-# LANGUAGE RankNTypes #-}
{-|
Module      : MyPrelude.Lens
Description : Simplified lens when you don't want to include all of lens
Copyright   : (c) Mats Rauhala, 2019
License     : MIT
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

A really small subset of lens for when you don't want to depend on the entirety of the lens ecosystem.

See documentation for 'Control.Lens'
-}
module MyPrelude.Lens
  (
    Getting
  , ASetter
  , Lens
  , Lens'
  , lens
  , over
  , set
  , view
  )
  where

import           Data.Functor.Const
import           Data.Functor.Identity
import           MyPrelude             (MonadReader, asks, ( #. ))

-- Getting type
type Getting r s a = (a -> Const r a) -> s -> Const r s

type ASetter s t a b = (a -> Identity b) -> s -> Identity t

-- A lens
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- Simplified lens
type Lens' s a = Lens s s a a

view :: MonadReader s m => Getting a s a -> m a
view l = asks (getConst #. l Const)
{-# INLINE view #-}

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity #. l (Identity #. f)
{-# INLINE over #-}

set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity #. l (\_ -> Identity b)
{-# INLINE set #-}
