{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Unsafe #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: internal
-- Portability: non-portable
--
-- Unsafe and unstable internals.
module System.Path.Internal where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Monoid (Monoid(..), Endo(..), (<>))
import Data.String (IsString(..))
import Data.Text (Text)
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.Handle (noNewlineTranslation)
import qualified Data.Text.Encoding.Locale as Text

#ifdef __POSIX__
import qualified Data.ByteString as ByteString
import qualified System.Posix.ByteString as Posix
import qualified System.Posix.FilePath as Posix
#else
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.Directory as FilePath
import qualified System.FilePath as FilePath
#endif

-- * Name

-- | A named component of a path, to be encoded or decoded as appropriate.
data Name = ByteString !ByteString | Text !Text deriving (Show)

instance IsString Name where
    fromString = Text . fromString

-- * Path

-- | The kind of types of vertices in the filesystem graph, and the objects of
-- the 'Path' category.
data Vertex = Root | Drive | Remote | Home | Working | Directory | File

-- | Infix 'Edge' type operator.
type (->-) = Edge

-- | The edges between vertices in the filesystem graph.
data Edge :: Vertex -> Vertex -> * where
    RootDirectory :: Root ->- Directory
    DriveName :: !Name -> Drive ->- Directory
    HostName :: !Name -> Remote ->- Directory
    HomeDirectory :: Home ->- Directory
    WorkingDirectory :: Working ->- Directory
    DirectoryName :: !Name -> Directory ->- Directory
    FileName :: !Name -> Directory ->- File
    FileExtension :: !Name -> File ->- File

deriving instance Show (Edge a b)

-- | Infix 'Path' type operator.
type (</>) = Path

-- | A path between vertices in the filesystem graph as a sequence of edges,
-- forming a free category under concatenation.
data Path :: Vertex -> Vertex -> * where
    Nil :: a </> a
    Cons :: a ->- b -> b </> c -> a </> c

deriving instance Show (Path a b)

instance (b ~ a) => Monoid (Path a b) where
    mempty = Nil
    mappend = (</>)

instance (a ~ Directory, b ~ a) => IsString (Path a b) where
    fromString = dir . fromString

-- * Combinators

-- | Concatenate two paths.
(</>) :: a </> b -> b </> c -> a </> c
Nil </> b = b
Cons e a </> b = Cons e (a </> b)

-- | Prepend a 'drive' name to a path.
(<:>) :: Name -> Directory </> b -> Drive </> b
n <:> b = drive n </> b

-- | Append a file extension to a file path.
(<.>) :: a </> File -> Name -> a </> File
a <.> n = a </> ext n

-- | Construct a 'Path' from an 'Edge'.
edge :: a ->- b -> a </> b
edge = flip Cons Nil

-- | The root directory.
root :: Root </> Directory
root = edge RootDirectory

-- | A drive letter / device name.
drive :: Name -> Drive </> Directory
drive = edge . DriveName

-- | A remote host.
host :: Name -> Remote </> Directory
host = edge . HostName

-- | The home directory of the current user.
home :: Home </> Directory
home = edge HomeDirectory

-- | The current working directory.
cwd :: Working </> Directory
cwd = edge WorkingDirectory

-- | A directory.
dir :: Name -> Directory </> Directory
dir = edge . DirectoryName

-- | A file name.
file :: Name -> Directory </> File
file = edge . FileName

-- | A file extension.
ext :: Name -> File </> File
ext = edge . FileExtension

-- * Builder

-- | Segments of a path representation.
data Chunk
    = ASCII !ByteString
      -- ^ Pass-through on encode; decode as ASCII.
    | Unicode !Text
      -- ^ Encode as appropriate; pass-through on decode.
    | Name !Name
      -- ^ Validate and encode/decode as appropriate.
  deriving (Show)

-- | Difference list monoid for assembling chunks efficiently.
newtype Builder = Builder (Endo [Chunk]) deriving (Monoid)

instance Show Builder where
    show = show . runBuilder

-- | Apply a 'Builder' to construct the final 'Chunk' list.
runBuilder :: Builder -> [Chunk]
runBuilder (Builder endo) = appEndo endo []

-- | The singleton 'Builder' primitive.
chunk :: Chunk -> Builder
chunk c = Builder (Endo (c :))

-- | Construct a 'Builder' for a 'ASCII' 'Chunk'.
ascii :: ByteString -> Builder
ascii = chunk . ASCII

-- | Construct a 'Builder' for a 'Unicode' 'Chunk'.
unicode :: Text -> Builder
unicode = chunk . Unicode

-- | Construct a 'Builder' for a 'Name' 'Chunk'.
name :: Name -> Builder
name = chunk . Name

-- * Representations

-- | Syntax for representing a path in readable form.
data Representation = Posix | Windows

-- | The native 'Representation' of the platform.
native :: Representation
#ifdef __WINDOWS__
native = Windows
#else
native = Posix
#endif

-- | Render a path to a 'Representation' intended for humans.
render :: Representation -> a </> b -> Builder
render _ Nil = mempty
render repr (Cons e a) =
    r repr e <> render repr a
  where
    r Posix RootDirectory = ascii "/"
    r Windows RootDirectory = unicode "\\"
    r Posix (DriveName n) = ascii "/" <> name n <> ascii ":" <> ascii "/"
    r Windows (DriveName n) = name n <> unicode ":\\"
    r Posix (HostName n) = name n <> ascii ":" <> ascii "/"
    r Windows (HostName n) = unicode "\\\\" <> name n <> unicode "\\"
    r Posix HomeDirectory = ascii "~" <> ascii "/"
    r Windows HomeDirectory = unicode "%UserProfile%\\"
    r Posix WorkingDirectory = ascii "." <> ascii "/"
    r Windows WorkingDirectory = unicode ".\\"
    r Posix (DirectoryName n) = name n <> ascii "/"
    r Windows (DirectoryName n) = name n <> unicode "\\"
    r _ (FileName n) = name n
    r Posix (FileExtension n) = ascii "." <> name n
    r Windows (FileExtension n) = unicode "." <> name n

-- | Render only the 'Directory' and 'File' resources of a path to the native
-- 'Representation' intended for machines.
path :: a </> b -> Builder
path Nil = mempty
path (Cons e a) =
    r e <> path a
  where
    r (DirectoryName _) = render native (edge e)
    r (FileName _) = render native (edge e)
    r (FileExtension _) = render native (edge e)
    r _ = mempty

-- * PathName

-- | A concrete path on the local file system.
newtype PathName = PathName
#ifdef __POSIX__
    Posix.RawFilePath
#else
    FilePath.FilePath
#endif
  deriving (Show)

-- | Append a 'PathName' to another.
append :: PathName -> PathName -> PathName
append (PathName a) (PathName b) = PathName (a <> b)

-- | Ensure that a 'PathName' ends with a path separator.
addTrailingPathSeparator :: PathName -> PathName
addTrailingPathSeparator (PathName a) =
#ifdef __POSIX__
    PathName (Posix.addTrailingPathSeparator a)
#else
    PathName (FilePath.addTrailingPathSeparator a)
#endif

-- | Construct a 'PathName' from a 'Builder' using the system locale.
locale :: Builder -> IO PathName
locale builder = do
    encoding <- getLocaleEncoding
#ifdef __POSIX__
    let c (ASCII b) = return b
        c (Unicode t) = Text.encodeLocale' encoding noNewlineTranslation t
        c (Name (ByteString b)) = return b
        c (Name (Text t)) = c (Unicode t)
    PathName . ByteString.concat <$> mapM c (runBuilder builder)
#else
    let c (ASCII b) = return (Text.decodeLatin1 b)
        c (Unicode t) = return t
        c (Name (ByteString b)) =
            Text.decodeLocale' encoding noNewlineTranslation b
        c (Name (Text t)) = return t
    PathName . Text.unpack . Text.concat <$> mapM c (runBuilder builder)
#endif

-- * Reify

-- | 'Vertex' types that we can 'reify'.  This is necessary to prevent things
-- like @reify (Nil :: Drive \</\> Drive)@ which would otherwise type-check
-- and return an empty 'PathName', which isn't a valid file path and certainly
-- not the name of a 'drive'.
class Reifiable (b :: Vertex)
instance Reifiable Directory
instance Reifiable File

-- | Reify an abstract 'Path' to a concrete 'PathName'.
class Reify a where
    reify :: (Reifiable b) => a </> b -> IO PathName

#ifndef __WINDOWS__
instance Reify Root where
    reify = locale . render Posix
#endif

#ifdef __WINDOWS__
instance Reify Drive where
    reify = locale . render Windows
#endif

instance Reify Home where
    reify p = do
#ifdef __POSIX__
        Just homeDir <- Posix.getEnv "HOME"
#else
        homeDir <- FilePath.getHomeDirectory
#endif
        pathName <- locale (path p)
        return (addTrailingPathSeparator (PathName homeDir) `append` pathName)

instance Reify Working where
    reify p = do
#ifdef __POSIX__
        workingDir <- Posix.getWorkingDirectory
#else
        workingDir <- FilePath.getCurrentDirectory
#endif
        pathName <- locale (path p)
        return (addTrailingPathSeparator (PathName workingDir)
            `append` pathName)
