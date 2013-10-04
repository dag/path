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
    , type (</>)
    , Edge(..)
    , type (->-)
    , Vertex(..)
    , IsRoot
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
      -- ** Reify
    , Reify(..)
    ) where

import System.Path.Internal
