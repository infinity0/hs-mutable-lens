module Control.Lens.Mutable
  ( -- ** Foundations
    PrimOpGroup(..)
  , S(..)
  , LST
  , MonadLST(..)
  , SLens
  -- ** Convenience utilities
  , runSLens
  , stateRead
  , stateWrite
  , stateModify
  -- ** Specific lens for various mutable reference types
  , AsLens(..)
  )
where

import           Control.Lens.Mutable.Internal
import           Control.Lens.Mutable.Types
