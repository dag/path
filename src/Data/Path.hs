{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
module Data.Path
    ( -- * Path
      type (</>)
    , Path(..)
    , Component(..)
    , Name(..)
      -- ** Combinators
    , root
    , drive
    , host
    , home
    , cwd
    , dir
    , file
    , ext
      -- ** Operators
    , (</>)
    , (<:>)
    , (<.>)
      -- * PathName
    , PathName
      -- ** Resolve
    , Resolve(..)
    ) where

import Data.Path.Internal
