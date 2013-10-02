{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Safe #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
--
-- Simplified public API.  See "Data.Path.Core" for the full API.
module Data.Path
    ( -- * Path
      Path
    , type (</>)
    , Vertex(..)
    , Name(..)
    , root
    , drive
    , host
    , home
    , cwd
    , dir
    , file
    , ext
    , (</>)
    , (<:>)
    , (<.>)
      -- * PathName
    , PathName
    , Reify(..)
    , Reifiable
    ) where

import Data.Path.Core
