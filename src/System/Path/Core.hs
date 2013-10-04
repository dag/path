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
      -- ** Operators
    , (</>)
    , (<:>)
    , (<.>)
      -- ** Combinators
    , root
    , drive
    , host
    , home
    , cwd
    , dir
    , file
    , ext
      -- * PathName
    , PathName
    , Reify(..)
    ) where

import System.Path.Internal
