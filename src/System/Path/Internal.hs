{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: internal
-- Portability: non-portable
module System.Path.Internal where

import Control.Applicative ((<$>), (<*>), pure)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy.Builder (byteString, char7)
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Encoding.Locale (encodeLocale', decodeLocale')
import Data.Text.Lazy.Builder (fromText)
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.Handle (noNewlineTranslation)
import Prelude hiding ((.), id)
import qualified Data.ByteString.Lazy.Builder as ByteString
import qualified Data.Text.Lazy.Builder as Text
import qualified Prelude
import qualified System.Posix.ByteString as Posix

-- * Types

-- | Poly-kinded @Category@.
class Category cat where
    id :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
    id = Prelude.id
    (.) = (Prelude..)

-- | Abstract path morphisms.
data Path :: Node -> Node -> * where
    Edge :: a </> a
    Path :: a </> b -> b </> c -> a </> c
    RootDirectory :: RootTree </> DirectoryTree
    DriveName :: !Component -> DriveTree </> DirectoryTree
    HostName :: !Component -> RemoteTree </> DirectoryTree
    HomeDirectory :: HomeTree </> DirectoryTree
    WorkingDirectory :: WorkingTree </> DirectoryTree
    DirectoryName :: !Component -> Tree t </> DirectoryTree
    FileName :: !Component -> Tree t </> File
    FileExtension :: !Component -> File </> File

deriving instance Show (Path a b)

instance (b ~ a) => Monoid (Path a b) where
    mempty = id
    mappend = (.)

instance Category Path where
    id = Edge
    Edge . a = a
    b . Edge = b
    b . a = Path a b

instance (a ~ DirectoryTree, b ~ a) => IsString (Path a b) where
    fromString = dir . fromString

-- | Infix type operator for 'Path'.
type (</>) = Path

-- | An abstract directory tree, not anchored to any root reference.
type DirectoryTree = Tree Directory

-- | The root directory tree on a POSIX filesystem.
type RootTree = Tree Root

-- | The root directory tree of a drive on a Windows filesystem.
type DriveTree = Tree Drive

-- | The root directory tree on a remote machine.
type RemoteTree = Tree Remote

-- | The home directory tree of the current user.
type HomeTree = Tree Home

-- | The current working directory tree.
type WorkingTree = Tree Working

-- | Data kind representing whether a 'Path' is absolute or relative, and in
-- reference to what, if anything.
data Reference = Directory | Root | Drive | Remote | Home | Working

-- | Data kind representing whether a 'Path' is a leaf ('File') or a branch
-- ('Tree').
data Node = File | Tree Reference

-- | Components of a 'Path' can either be raw bytes, or unicode using the
-- system locale encoding.
data Component = ByteString !ByteString | Text !Text deriving Show

instance IsString Component where
    fromString = Text . fromString

-- * Operations

-- | Build a POSIX-compatible absolute path.
posix :: (Posix r) => Tree r </> b -> IO ByteString.Builder
posix path = do
    locale <- getLocaleEncoding
    let encode (ByteString b) = pure (byteString b)
        encode (Text t) =
            byteString <$> encodeLocale' locale noNewlineTranslation t
        build :: a </> b -> IO ByteString.Builder
        build = \case
            Edge -> pure mempty
            Path a b -> mappend <$> build a <*> build b
            RootDirectory -> pure (char7 '/')
            DriveName c -> do
                b <- encode c
                return (char7 '/' <> b <> char7 ':')
            HostName c -> mappend <$> encode c <*> pure (char7 ':')
            HomeDirectory -> do
                Just homeDir <- Posix.getEnv "HOME"
                return (byteString homeDir <> char7 '/')
            WorkingDirectory -> do
                workingDir <- Posix.getWorkingDirectory
                return (byteString workingDir <> char7 '/')
            DirectoryName c -> mappend <$> encode c <*> pure (char7 '/')
            FileName c -> encode c
            FileExtension c -> mappend <$> pure (char7 '.') <*> encode c
    build path

-- | References that we can resolve on a POSIX system.
class Posix (r :: Reference)
instance Posix Directory
instance Posix Root
instance Posix Home
instance Posix Working

-- | Build a Windows-compatible absolute path.
windows :: (Windows r) => Tree r </> b -> IO Text.Builder
windows path = do
    locale <- getLocaleEncoding
    let decode (ByteString b) =
            fromText <$> decodeLocale' locale noNewlineTranslation b
        decode (Text t) = pure (fromText t)
        build :: a </> b -> IO Text.Builder
        build = \case
            Edge -> pure mempty
            Path a b -> mappend <$> build a <*> build b
            RootDirectory -> pure "\\"
            DriveName c -> mappend <$> decode c <*> pure ":\\"
            HostName c -> do
                b <- decode c
                return ("\\\\" <> b <> "\\")
            HomeDirectory -> do
                Just homeDir <- Posix.getEnv "HOME"
                b <- decode (ByteString homeDir)
                return (b <> "\\")
            WorkingDirectory -> do
                workingDir <- Posix.getWorkingDirectory
                b <- decode (ByteString workingDir)
                return (b <> "\\")
            DirectoryName c -> mappend <$> decode c <*> pure "\\"
            FileName c -> decode c
            FileExtension c -> mappend <$> pure "." <*> decode c
    build path

-- | References that we can resolve on a Windows system.
class Windows (r :: Reference)
instance Windows Directory
instance Windows Drive
instance Windows Home
instance Windows Working

-- * Combinators

-- | Append a 'Path' to a directory.
(</>) :: a </> b -> b </> c -> a </> c
(</>) = flip (.)

-- | Append a 'Path' to a 'drive' name.
(<:/>) :: Component -> DirectoryTree </> b -> DriveTree </> b
d <:/> b = drive d </> b

-- | Append a 'Path' to a 'host' name.
(<@/>) :: Component -> DirectoryTree </> b -> RemoteTree </> b
c <@/> p = host c </> p

-- | Append a file extension to a file 'Path'.
(<.>) :: a </> File -> Component -> a </> File
a <.> e = ext e . a

-- | The root directory.
root :: RootTree </> DirectoryTree
root = RootDirectory

-- | A drive letter / device name.
drive :: Component -> DriveTree </> DirectoryTree
drive = DriveName

-- | A remote host.
host :: Component -> RemoteTree </> DirectoryTree
host = HostName

-- | The home directory of the current user.
home :: HomeTree </> DirectoryTree
home = HomeDirectory

-- | The current working directory.
cwd :: WorkingTree </> DirectoryTree
cwd = WorkingDirectory

-- | A directory.
dir :: Component -> DirectoryTree </> DirectoryTree
dir = DirectoryName

-- | A file name.
file :: Component -> DirectoryTree </> File
file = FileName

-- | A file extension.
ext :: Component -> File </> File
ext = FileExtension
