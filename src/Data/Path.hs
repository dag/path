{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
module Data.Path
    ( -- * Types
      type (</>)
    , Path(..)
    , Node(..)
    , Name(..)
      -- * Combinators
    , root
    , drive
    , host
    , home
    , cwd
    , dir
    , file
    , ext
      -- * Operators
    , (</>)
    , (<:>)
    , (<.>)
    ) where

import Data.Path.Internal
