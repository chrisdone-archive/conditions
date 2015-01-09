{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

-- | Conditions system.

module Control.Condition where

import Control.Exception
import Data.Typeable
import Unsafe.Coerce

-- | A list of handlers.
type Handlers = (?handlers :: [ConditionHandler])

-- | A generic condition handler.
data ConditionHandler where
  ConditionHandler :: Condition c r => (c -> r) -> ConditionHandler

-- | A condition must have a label type
class (Exception c) => Condition c r | c -> r

-- | Signal a condition.
signal :: (Handlers,Condition c r) => c -> r
signal c = go ?handlers
  where go (ConditionHandler f:hs) =
          case cast c of
            Just c' -> unsafeCoerce (f c')
            Nothing -> go hs
        go [] = throw c

-- | Handle the given condition.
handler :: (Handlers,Condition c r)
        => (Handlers => c -> r) -- ^ Condition handler.
        -> (Handlers => x)      -- ^ Scope of the condition handler.
        -> x
handler h r =
  let ?handlers = ConditionHandler h : old
  in r
  where old = ?handlers

-- | Start the conditions system.
withConditions :: (Handlers => r) -> r
withConditions x =
  let ?handlers = []
  in x
