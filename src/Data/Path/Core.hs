{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
--
-- The complete public API.
module Data.Path.Core
    ( -- * Path
      Path(..)
    , type (</>)
    , Edge(..)
    , type (->-)
    , Vertex(..)
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
    , Reifiable
    ) where

import Data.Path.Internal
