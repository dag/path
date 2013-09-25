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

-- | Components of a 'Path'.
data Component = Root | Drive | Remote | Home | Working | Directory | File

-- | Named components of a path, to be either encoded or decoded as
-- appropriate, or taken verbatim.
data Name = ByteString !ByteString | Text !Text deriving (Show)

instance IsString Name where
    fromString = Text . fromString

-- | Path primitives.
data Path :: Component -> Component -> * where
    RootDirectory :: Path Root Directory
    DriveName :: !Name -> Path Drive Directory
    HostName :: !Name -> Path Remote Directory
    HomeDirectory :: Path Home Directory
    WorkingDirectory :: Path Working Directory
    DirectoryName :: !Name -> Path Directory Directory
    FileName :: !Name -> Path Directory File
    FileExtension :: !Name -> Path File File

deriving instance Show (Path a b)

-- | Abstract path as a linked list of 'Path' primitives.
data (</>) :: Component -> Component -> * where
    End :: a </> a
    Path :: Path a b -> b </> c -> a </> c

deriving instance Show (a </> b)

instance (a ~ Directory, b ~ a) => IsString (a </> b) where
    fromString = dir . fromString

-- * Combinators

-- | Join two paths together.
(</>) :: a </> b -> b </> c -> a </> c
End </> b = b
Path p a </> b = Path p (a </> b)

-- | Append a path to a 'drive' name.
(<:>) :: Name -> Directory </> b -> Drive </> b
n <:> b = drive n </> b

-- | Append a file extension to a file path.
(<.>) :: a </> File -> Name -> a </> File
a <.> n = a </> ext n

-- | The root directory.
root :: Root </> Directory
root = Path RootDirectory End

-- | A drive letter / device name.
drive :: Name -> Drive </> Directory
drive = flip Path End . DriveName

-- | A remote host.
host :: Name -> Remote </> Directory
host = flip Path End . HostName

-- | The home directory of the current user.
home :: Home </> Directory
home = flip Path End HomeDirectory

-- | The current working directory.
cwd :: Working </> Directory
cwd = flip Path End WorkingDirectory

-- | A directory.
dir :: Name -> Directory </> Directory
dir = flip Path End . DirectoryName

-- | A file name.
file :: Name -> Directory </> File
file = flip Path End . FileName

-- | A file extension.
ext :: Name -> File </> File
ext = flip Path End . FileExtension

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

-- | Render a path to a 'Representation' intended for humans.
render :: Representation -> a </> b -> Builder
render _ End = mempty
render repr (Path p a) =
    r repr p <> render repr a
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

-- | Render only the 'Directory' and 'File' components of a path to the native
-- 'Representation' intended for machines.
path :: a </> b -> Builder
path End = mempty
path (Path p a) =
    r p <> path a
  where
    r (DirectoryName _) = render native (Path p End)
    r (FileName _) = render native (Path p End)
    r (FileExtension _) = render native (Path p End)
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

-- * Reify

-- | Reify an abstract path to a concrete 'PathName'.
class Reify (a :: Component) where
    reify :: a </> b -> IO PathName

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
        return (PathName homeDir <> pathName)

instance Reify Working where
    reify p = do
#ifdef __POSIX__
        workingDir <- Posix.getWorkingDirectory
#else
        workingDir <- FilePath.getCurrentDirectory
#endif
        pathName <- locale (path p)
        return (PathName workingDir <> pathName)
