{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
module Data.Path
    ( -- * Path
      Path(..)
    , type (</>)
    , Component(..)
    , type (/>)
    , Name(..)
    , Resource(..)
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
