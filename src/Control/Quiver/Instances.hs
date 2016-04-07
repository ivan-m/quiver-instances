{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
   Module      : Control.Quiver.Instances
   Description : Extra instances for Quiver
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Control.Quiver.Instances () where

import Control.Quiver.Internal
import Control.Quiver.SP

import Control.Monad       (join)
import Control.Monad.Catch

--------------------------------------------------------------------------------

-- | Throws exceptions into the base monad.
instance (MonadThrow f) => MonadThrow (P a a' b b' f) where
  throwM = qlift . throwM

instance (MonadCatch f) => MonadCatch (P a a' b b' f) where
  catch p onErr = go p
    where
      go p' = case p' of
                Consume x k q -> consume x (go . k) (decouple (go q))
                Produce y k q -> produce y (go . k) (deplete  (go q))
                Enclose m -> enclose (catch (go <$> m) (return . onErr))
                Deliver r -> deliver r
