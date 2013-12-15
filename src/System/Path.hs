{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Safe #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
--
-- Simplified public API.  See "System.Path.Core" for the full API.
module System.Path
    ( -- * Abstract path category
      -- $path
      Path
    , File(..)
    , Name(..)
    , type (</>)
    , (</>)
    , (<:>)
    , (<.>)
    , root
    , drive
    , host
    , home
    , cwd
    , dir
    , file
    , ext
      -- * Concrete resolved paths
    , ResolvedPath
    , Reference(..)
    , Resolve(..)
    ) where

import System.Path.Core

-- $path
--
-- A filesystem tree hierarchy is a directed graph in the form of a rooted
-- tree, and a path in the filesystem sense is a path in the graph theory sense
-- with the path separators being the edges and the path components the
-- vertices.  We model this as a free category with concatenation of paths,
-- which are sequences of edges, which in turn are ordered pairs of vertices.
