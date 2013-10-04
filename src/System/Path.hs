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
      -- * Opaque file system pathname
      -- $pathname
    , PathName
    , Reify(..)
    , Reifiable
    ) where

import System.Path.Core

-- $path
--
-- A file system tree hierarchy is a directed graph in the form of a rooted
-- tree, and a path in the file system sense is a path in the graph theory
-- sense with the path separators being the edges and the path components the
-- vertices.  We model this as a free category with concatenation of paths,
-- which are sequences of edges, which in turn are ordered pairs of vertices.

-- $pathname
--
-- To avoid the overhead of constantly parsing and rendering paths when
-- interacting with a file system, or the problem of mapping file system
-- entries to the 'Path' vertex types, we instead provide an opaque,
-- monomorphic type for concrete pathnames that uses an efficient internal
-- representation whenever possible and which can only be created by reifying a
-- 'Path' or given to us by the file system itself, or by combining two
-- pathnames into one (/TODO/).  This allows us to build pathnames efficiently
-- and safely, at the expense of expressive power.  To regain that power, we
-- can trade it for efficiency by parsing a 'PathName' back into a 'Path'
-- (/TODO/).
