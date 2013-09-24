{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: internal
-- Portability: non-portable
module Data.Path.Internal where

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

-- * Path

-- | Infix 'Path' type operator.
type (</>) = Path

-- | Components of a 'Path'.
data Component = Root | Drive | Remote | Home | Working | Directory | File

-- | An abstract path from one 'Component' to another.
data Path :: Component -> Component -> * where
    Path :: a </> b -> b </> c -> a </> c
    RootDirectory :: Root </> Directory
    DriveName :: !Name -> Drive </> Directory
    HostName :: !Name -> Remote </> Directory
    HomeDirectory :: Home </> Directory
    WorkingDirectory :: Working </> Directory
    DirectoryName :: !Name -> Directory </> Directory
    FileName :: !Name -> Directory </> File
    FileExtension :: !Name -> File </> File

deriving instance Show (Path a b)

instance (a ~ Directory, b ~ a) => IsString (Path a b) where
    fromString = dir . fromString

-- | Named components of a path, to be either encoded or decoded as
-- appropriate, or taken verbatim.
data Name = ByteString !ByteString | Text !Text deriving (Show)

instance IsString Name where
    fromString = Text . fromString

-- * Combinators

-- | Join two paths together.
(</>) :: a </> b -> b </> c -> a </> c
(</>) = Path

-- | Append a 'Path' to a 'drive' name.
(<:>) :: Name -> Directory </> b -> Drive </> b
n <:> b = drive n </> b

-- | Append a file extension to a file 'Path'.
(<.>) :: a </> File -> Name -> a </> File
a <.> n = a </> ext n

-- | The root directory.
root :: Root </> Directory
root = RootDirectory

-- | A drive letter / device name.
drive :: Name -> Drive </> Directory
drive = DriveName

-- | A remote host.
host :: Name -> Remote </> Directory
host = HostName

-- | The home directory of the current user.
home :: Home </> Directory
home = HomeDirectory

-- | The current working directory.
cwd :: Working </> Directory
cwd = WorkingDirectory

-- | A directory.
dir :: Name -> Directory </> Directory
dir = DirectoryName

-- | A file name.
file :: Name -> Directory </> File
file = FileName

-- | A file extension.
ext :: Name -> File </> File
ext = FileExtension

-- * Builder

-- | Difference list monoid for assembling chunks efficiently.
newtype Builder = Builder (Endo [Chunk]) deriving (Monoid)

instance Show Builder where
    show = show . runBuilder

-- | Apply a 'Builder' to construct the final 'Chunk' list.
runBuilder :: Builder -> [Chunk]
runBuilder (Builder endo) = appEndo endo []

-- | Segments of a path representation.
data Chunk
    = ASCII !ByteString
      -- ^ Pass-through on encode; decode as ASCII.
    | Unicode !Text
      -- ^ Encode as appropriate; pass-through on decode.
    | Name !Name
      -- ^ Validate and encode/decode as appropriate.
  deriving (Show)

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

-- | Render a 'Path' to a 'Representation' intended for humans.
render :: Representation -> a </> b -> Builder
render r (Path a b) = render r a <> render r b
render Posix RootDirectory = ascii "/"
render Windows RootDirectory = unicode "\\"
render Posix (DriveName n) = ascii "/" <> name n <> ascii ":" <> ascii "/"
render Windows (DriveName n) = name n <> unicode ":\\"
render Posix (HostName n) = name n <> ascii ":" <> ascii "/"
render Windows (HostName n) = unicode "\\\\" <> name n <> unicode "\\"
render Posix HomeDirectory = ascii "~" <> ascii "/"
render Windows HomeDirectory = unicode "%UserProfile%\\"
render Posix WorkingDirectory = ascii "." <> ascii "/"
render Windows WorkingDirectory = unicode ".\\"
render Posix (DirectoryName n) = name n <> ascii "/"
render Windows (DirectoryName n) = name n <> unicode "\\"
render _ (FileName n) = name n
render Posix (FileExtension n) = ascii "." <> name n
render Windows (FileExtension n) = unicode "." <> name n

-- | Render only the 'Directory' and 'File' components of a 'Path' to the
-- native 'Representation' intended for machines.
path :: a </> b -> Builder
path p = case p of
    Path a b -> path a <> path b
    DirectoryName _ -> r p
    FileName _ -> r p
    FileExtension _ -> r p
    _ -> mempty
  where
    r = render native

-- * PathName

-- | A concrete path on the local file system.
newtype PathName = PathName
#ifdef __POSIX__
    Posix.RawFilePath
#else
    FilePath.FilePath
#endif
  deriving (Show)

instance Monoid PathName where
    mempty = PathName mempty
#ifdef __POSIX__
    mappend (PathName a) (PathName b) = PathName (Posix.combine a b)
#else
    mappend (PathName a) (PathName b) = PathName (FilePath.combine a b)
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

-- * Resolve

-- | Resolve an abstract 'Path' to a concrete 'PathName'.
class Resolve (a :: Component) where
    resolve :: a </> b -> IO PathName

#ifndef __WINDOWS__
instance Resolve Root where
    resolve = locale . render Posix
#endif

#ifdef __WINDOWS__
instance Resolve Drive where
    resolve = locale . render Windows
#endif

instance Resolve Home where
    resolve p = do
#ifdef __POSIX__
        Just homeDir <- Posix.getEnv "HOME"
#else
        homeDir <- FilePath.getHomeDirectory
#endif
        pathName <- locale (path p)
        return (PathName homeDir <> pathName)

instance Resolve Working where
    resolve p = do
#ifdef __POSIX__
        workingDir <- Posix.getWorkingDirectory
#else
        workingDir <- FilePath.getCurrentDirectory
#endif
        pathName <- locale (path p)
        return (PathName workingDir <> pathName)
