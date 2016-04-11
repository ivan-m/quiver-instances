{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes,
             UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
   Module      : Control.Quiver.Instances
   Description : Extra instances for Quiver
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

Currently this provides instances for the 'MonadThrow', 'MonadCatch'
and 'MonadMask' classes from the
<http://hackage.haskell.org/package/exceptions exceptions> library.

 -}
module Control.Quiver.Instances () where

import Control.Quiver.Internal

import Control.Monad.Base           (MonadBase (..), liftBaseDefault)
import Control.Monad.Catch
import Control.Monad.IO.Class       (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource (..))
import Data.IORef                   (newIORef, readIORef, writeIORef)

--------------------------------------------------------------------------------
-- Instances for classes from the exceptions library.

-- | Throws exceptions into the base monad.
instance (MonadThrow f) => MonadThrow (P a a' b b' f) where
  throwM = qlift . throwM

instance (MonadCatch f) => MonadCatch (P a a' b b' f) where
  catch p onErr = go p
    where
      go p' = case p' of
                Consume x k q -> consume x (go . k) (decouple (go q))
                Produce y k q -> produce y (go . k) (deplete  (go q))
                Enclose m     -> enclose (catch (go <$> m) (return . onErr))
                Deliver r     -> deliver r

-- Based upon code in pipes-safe

instance (MonadIO f, MonadMask f) => MonadMask (P a a' b b' f) where
  mask = liftMask mask

  uninterruptibleMask = liftMask uninterruptibleMask

data Restore m = Unmasked | Masked (forall x . m x -> m x)

liftMask :: (MonadIO f, MonadMask f)
            => (forall s. ((forall x. f x -> f x) -> f s) -> f s)
            -> ((forall x. P a a' b b' f x -> P a a' b b' f x)
                -> P a a' b b' f r)
            -> P a a' b b' f r
liftMask maskVariant pk = do
  ioref <- liftIO $ newIORef Unmasked

  let
    -- Mask actions in the base monad
    maskM p = case p of
                  Consume x k q -> consume x (maskM . k) (decouple (maskM q))
                  Produce y k q -> produce y (maskM . k) (deplete  (maskM q))
                  Enclose m     -> enclose $ maskVariant $ \unmaskVariant -> do
                    -- stash base's unmask and merge action
                    liftIO $ writeIORef ioref $ Masked unmaskVariant
                    maskM <$> (m >>= mergeAdjacent)
                  Deliver r     -> deliver r

    unmask p = case p of
                 Consume x k q -> consume x (unmask . k) (decouple (unmask q))
                 Produce y k q -> produce y (unmask . k) (deplete  (unmask q))
                 Enclose m     -> enclose $ do
                   -- retrieve base's unmask and apply to merged action
                   Masked unmaskVariant <- liftIO $ readIORef ioref
                   unmaskVariant (unmask <$> (m >>= mergeAdjacent))
                 Deliver r     -> deliver r

    mergeAdjacent p = case p of
                        Enclose m -> m >>= mergeAdjacent
                        _         -> return p
  maskM (pk unmask)

--------------------------------------------------------------------------------
-- Instances for classes from the transformers-base library.

-- This requires UndecidableInstances
instance (MonadBase bm f) => MonadBase bm (P a a' b b' f) where
  liftBase = liftBaseDefault

--------------------------------------------------------------------------------
-- Instances for classes from the resourcet library.

instance (MonadResource f) => MonadResource (P a a' b b' f) where
  liftResourceT = qlift . liftResourceT
