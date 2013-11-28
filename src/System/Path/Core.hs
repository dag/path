{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
--
-- The complete public API.
module System.Path.Core
    ( -- * Path
      Path(..)
    , Edge(..)
    , Vertex(..)
    , Name(..)
      -- ** Type level
    , type (</>)
    , type (->-)
    , IsRoot
    , Rooted
    , NonEmpty
    , IsNative
    , Native
      -- ** Operators
    , (</>)
    , (<:>)
    , (<.>)
      -- ** Combinators
    , path
    , root
    , drive
    , host
    , home
    , cwd
    , dir
    , file
    , ext
      -- * ResolvedPath
    , ResolvedPath
    , Reference(..)
    , Resolve(..)
    ) where

import System.Path.Internal
