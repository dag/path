{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: internal
-- Portability: non-portable
module System.Path.Internal where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text.Encoding.Locale as Text
import qualified System.Posix.ByteString as Posix
import qualified System.Posix.FilePath as Posix

-- * Types

-- | Data kind representing whether a 'Path' is absolute or relative, and to
-- what.
data Reference = Orphan | Root | Drive | Remote | Home | Working

-- | Data kind representing whether a 'Path' is a branch ('Directory') or a
-- leaf ('File').
data Node = Directory | File

-- | Data kind representing whether a 'Path' 'Component' has a mix of 'Text'
-- and 'ByteString' or if it  has been encoded or decoded using the system
-- locale.
data Encoding = Mixed | Encoded | Decoded

-- | A 'Path' with 'Mixed' 'Encoding' may contain both 'Text' and 'ByteString'
-- components; otherwise, the components have been encoded or decoded with the
-- system locale.
data Component :: Encoding -> * where
    ByteString :: !ByteString -> Component Mixed
    Text :: !Text -> Component Mixed
    Encode :: !ByteString -> Component Encoded
    Decode :: !Text -> Component Decoded

deriving instance Show (Component e)

instance (e ~ Mixed) => IsString (Component e) where
    fromString = Text . fromString

-- | An absolute or relative path to a file or directory on a local filesystem
-- or a remote host, possibly with a mixture of unknown encodings and the
-- system locale.
data Path :: Reference -> Node -> Encoding -> * where
    Path :: !(Path r Directory e) -> !(Path Orphan n e) -> Path r n e
    Extension :: !(Path r File e) -> !(Component e) -> Path r File e
    OrphanDirectory :: Path Orphan Directory e
    RootDirectory :: Path Root Directory e
    DriveName :: !(Component e) -> Path Drive Directory e
    HostName :: !(Component e) -> Path Remote Directory e
    HomeDirectory :: Path Home Directory e
    WorkingDirectory :: Path Working Directory e
    DirectoryName :: !(Component e) -> Path Orphan Directory e
    FileName :: !(Component e) -> Path Orphan File e

deriving instance Show (Path r n e)

instance (r ~ Orphan, n ~ Directory) => Monoid (Path r n e) where
    mempty = orphan
    mappend = (</>)

instance (r ~ Orphan, n ~ Directory, e ~ Mixed) => IsString (Path r n e) where
    fromString = dir . fromString

-- * Operations

-- | Resolve references in a 'Path'.
class Absolute r where
    absolute :: Path r n Mixed -> IO (Path Root n Mixed)

instance Absolute Root where
    absolute = return

instance Absolute Home where
    absolute path = do
        Just homeDir <- Posix.getEnv "HOME"
        let dirs = tail (Posix.splitDirectories homeDir)
            rootPath = mconcat (map (DirectoryName . ByteString) dirs)
        return (rootPath <//> relative path)
      where
        relative :: Path Home n e -> Path Orphan n e
        relative HomeDirectory = orphan
        relative (Path p p') = Path (relative p) p'
        relative (Extension p c) = Extension (relative p) c

instance Absolute Working where
    absolute path = do
        workingDir <- Posix.getWorkingDirectory
        let dirs = tail (Posix.splitDirectories workingDir)
            rootPath = mconcat (map (DirectoryName . ByteString) dirs)
        return (rootPath <//> relative path)
      where
        relative :: Path Working n e -> Path Orphan n e
        relative WorkingDirectory = orphan
        relative (Path p p') = Path (relative p) p'
        relative (Extension p c) = Extension (relative p) c

-- | Encode the 'Text' components using the system locale.
encode :: Path r n Mixed -> IO (Path r n Encoded)
encode path = case path of
    Path p p' -> Path <$> encode p <*> encode p'
    Extension p m -> encode p >>= encoded m . Extension
    OrphanDirectory -> return OrphanDirectory
    RootDirectory -> return RootDirectory
    DriveName m -> encoded m DriveName
    HostName m -> encoded m HostName
    HomeDirectory -> return HomeDirectory
    WorkingDirectory -> return WorkingDirectory
    DirectoryName m -> encoded m DirectoryName
    FileName m -> encoded m FileName
  where
    encoded :: Component Mixed -> (Component Encoded -> a) -> IO a
    encoded (ByteString b) f = f . Encode <$> return b
    encoded (Text t) f = f . Encode <$> Text.encodeLocale t

-- | Decode the 'ByteString' components using the system locale.
decode :: Path r n Mixed -> IO (Path r n Decoded)
decode path = case path of
    Path p p' -> Path <$> decode p <*> decode p'
    Extension p m -> decode p >>= decoded m . Extension
    OrphanDirectory -> return OrphanDirectory
    RootDirectory -> return RootDirectory
    DriveName m -> decoded m DriveName
    HostName m -> decoded m HostName
    HomeDirectory -> return HomeDirectory
    WorkingDirectory -> return WorkingDirectory
    DirectoryName m -> decoded m DirectoryName
    FileName m -> decoded m FileName
  where
    decoded :: Component Mixed -> (Component Decoded -> a) -> IO a
    decoded (ByteString b) f = f . Decode <$> Text.decodeLocale b
    decoded (Text t) f = f . Decode <$> return t

-- | Render a 'Path' to a POSIX representation.
posix :: Path r n Encoded -> ByteString
posix path = case path of
    Path p p' -> posix p <> posix p'
    Extension p c -> posix p <> "." <> component c
    OrphanDirectory -> ""
    RootDirectory -> "/"
    DriveName c -> "/" <> component c <> ":" <> "/"
    HostName c -> component c <> ":/"
    HomeDirectory -> "~/"
    WorkingDirectory -> "./"
    DirectoryName c -> component c <> "/"
    FileName c -> component c
  where
    component :: Component Encoded -> ByteString
    component (Encode b) = b

-- | Render a 'Path' to a Windows representation.
windows :: Path r n Decoded -> Text
windows path = case path of
    Path p p' -> windows p <> windows p'
    Extension p c -> windows p <> "." <> component c
    OrphanDirectory -> ""
    RootDirectory -> "\\"
    DriveName c -> component c <> ":" <> "\\"
    HostName c -> "\\\\" <> component c <> "\\"
    HomeDirectory -> "%UserProfile%"
    WorkingDirectory -> ".\\"
    DirectoryName c -> component c <> "\\"
    FileName c -> component c
  where
    component :: Component Decoded -> Text
    component (Decode t) = t

-- * Combinators

-- | Append a 'Path' to a directory.
(</>) :: Path r Directory e -> Path Orphan n e -> Path r n e
p </> OrphanDirectory = p
p </> p' = Path p p'

-- | Append a 'Path' to a directory under 'root'.
(<//>) :: Path Orphan Directory e -> Path Orphan n e -> Path Root n e
p <//> p' = root </> p </> p'

-- | Append a 'Path' to a 'drive' name.
(<:/>) :: Component e -> Path Orphan n e -> Path Drive n e
c <:/> p = drive c </> p

-- | Append a 'Path' to a 'host' name.
(<@/>) :: Component e -> Path Orphan n e -> Path Remote n e
c <@/> p = host c </> p

-- | Append a 'Path' to a directory under 'home'.
(<~/>) :: Path Orphan Directory e -> Path Orphan n e -> Path Home n e
p <~/> p' = home </> p </> p'

-- | Append a file extension to a file 'Path'.
(<.>) :: Path r File e -> Component e -> Path r File e
p <.> c = Extension p c

-- | An \"orphan\" directory has no 'Reference' point.
orphan :: Path Orphan Directory e
orphan = OrphanDirectory

-- | The root directory.
root :: Path Root Directory e
root = RootDirectory

-- | A drive letter / device name.
drive :: Component e -> Path Drive Directory e
drive = DriveName

-- | A remote host.
host :: Component e -> Path Remote Directory e
host = HostName

-- | The home directory of the current user.
home :: Path Home Directory e
home = HomeDirectory

-- | The current working directory.
cwd :: Path Working Directory e
cwd = WorkingDirectory

-- | A directory.
dir :: Component e -> Path Orphan Directory e
dir = DirectoryName

-- | A file name.
file :: Component e -> Path Orphan File e
file = FileName
