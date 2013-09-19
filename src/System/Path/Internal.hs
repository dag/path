{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: internal
-- Portability: non-portable
module System.Path.Internal where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text.Encoding.Locale as Text
import qualified System.Posix.ByteString as Posix
import qualified System.Posix.FilePath as Posix

-- | Data kind representing whether a 'Path' is absolute or relative, and to
-- what.
data Reference = Current | Root | Drive | Remote | Home | Working

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
    CurrentDirectory :: Path Current Directory e
    RootDirectory :: Path Root Directory e
    DriveName :: !(Component e) -> Path Drive Directory e
    HostName :: !(Component e) -> Path Remote Directory e
    HomeDirectory :: Path Home Directory e
    WorkingDirectory :: Path Working Directory e
    DirectoryPath :: !(Path c Directory e) -> !(Component e) -> Path c Directory e
    FilePath :: !(Path c Directory e) -> !(Component e) -> Path c File e
    FileExtension :: !(Path c File e) -> !(Component e) -> Path c File e

deriving instance Show (Path r n e)

instance Monoid (Path Current Directory e) where
    mempty = cur
    mappend = (</>)

instance IsString (Path Current Directory Mixed) where
    fromString = dir . fromString

instance IsString (Path Current File Mixed) where
    fromString = file . fromString

-- | Resolve references in a 'Path'.
class Absolute r where
    absolute :: Path r n Mixed -> IO (Path Root n Mixed)

instance Absolute Root where
    absolute = return

instance Absolute Home where
    absolute path = do
        Just homeDir <- Posix.getEnv "HOME"
        let dirs = tail (Posix.splitDirectories homeDir)
            rootPath = mconcat (map (DirectoryPath cur . ByteString) dirs)
        return (rootPath <//> relative path)
      where
        relative :: Path Home n e -> Path Current n e
        relative HomeDirectory = cur
        relative (DirectoryPath p r) = DirectoryPath (relative p) r
        relative (FilePath p r) = FilePath (relative p) r
        relative (FileExtension p r) = FileExtension (relative p) r

instance Absolute Working where
    absolute path = do
        workingDir <- Posix.getWorkingDirectory
        let dirs = tail (Posix.splitDirectories workingDir)
            rootPath = mconcat (map (DirectoryPath cur . ByteString) dirs)
        return (rootPath <//> relative path)
      where
        relative :: Path Working n e -> Path Current n e
        relative WorkingDirectory = cur
        relative (DirectoryPath p r) = DirectoryPath (relative p) r
        relative (FilePath p r) = FilePath (relative p) r
        relative (FileExtension p r) = FileExtension (relative p) r

-- | Encode the 'Text' components using the system locale.
encode :: Path r n Mixed -> IO (Path r n Encoded)
encode path = case path of
    CurrentDirectory -> return CurrentDirectory
    RootDirectory -> return RootDirectory
    DriveName m -> encoded m DriveName
    HostName m -> encoded m HostName
    HomeDirectory -> return HomeDirectory
    WorkingDirectory -> return WorkingDirectory
    DirectoryPath p m -> encode p >>= encoded m . DirectoryPath
    FilePath p m -> encode p >>= encoded m . FilePath
    FileExtension p m -> encode p >>= encoded m . FileExtension
  where
    encoded :: Component Mixed -> (Component Encoded -> a) -> IO a
    encoded (ByteString b) f = f . Encode <$> return b
    encoded (Text t) f = f . Encode <$> Text.encodeLocale t

-- | Decode the 'ByteString' components using the system locale.
decode :: Path r n Mixed -> IO (Path r n Decoded)
decode path = case path of
    CurrentDirectory -> return CurrentDirectory
    RootDirectory -> return RootDirectory
    DriveName m -> decoded m DriveName
    HostName m -> decoded m HostName
    HomeDirectory -> return HomeDirectory
    WorkingDirectory -> return WorkingDirectory
    DirectoryPath p m -> decode p >>= decoded m . DirectoryPath
    FilePath p m -> decode p >>= decoded m . FilePath
    FileExtension p m -> decode p >>= decoded m . FileExtension
  where
    decoded :: Component Mixed -> (Component Decoded -> a) -> IO a
    decoded (ByteString b) f = f . Decode <$> Text.decodeLocale b
    decoded (Text t) f = f . Decode <$> return t

-- | Render a 'Path' to a POSIX representation.
posix :: Path r n Encoded -> ByteString
posix path = case path of
    CurrentDirectory -> ""
    RootDirectory -> "/"
    DriveName c -> "/" <> component c <> ":" <> "/"
    HostName c -> component c <> ":/"
    HomeDirectory -> "~/"
    WorkingDirectory -> "./"
    FilePath p c -> posix p <> component c
    DirectoryPath p c -> posix p <> component c <> "/"
    FileExtension p e -> posix p <> "." <> component e
  where
    component :: Component Encoded -> ByteString
    component (Encode b) = b

-- | Render a 'Path' to a Windows representation.
windows :: Path r n Decoded -> Text
windows path = case path of
    CurrentDirectory -> ""
    RootDirectory -> "\\"
    DriveName c -> component c <> ":" <> "\\"
    HostName c -> "\\\\" <> component c <> "\\"
    HomeDirectory -> "%UserProfile%"
    WorkingDirectory -> ".\\"
    FilePath p c -> windows p <> component c
    DirectoryPath p c -> windows p <> component c <> "\\"
    FileExtension p e -> windows p <> "." <> component e
  where
    component :: Component Decoded -> Text
    component (Decode t) = t

-- | Append a 'Path' to a directory.
(</>) :: Path r Directory e -> Path Current n e -> Path r n e
p </> CurrentDirectory = p
p </> DirectoryPath p' b = DirectoryPath (p </> p') b
p </> FilePath p' b = FilePath (p </> p') b
p </> FileExtension p' b = FileExtension (p </> p') b

-- | Append a 'Path' to a directory under 'root'.
(<//>) :: Path Current Directory e -> Path Current n e -> Path Root n e
p <//> p' = root </> p </> p'

-- | Append a 'Path' do a 'drive' name.
(<:/>) :: Component e -> Path Current n e -> Path Drive n e
c <:/> p = drive c </> p

-- | Append a 'Path' to a directory under 'home'.
(<~/>) :: Path Current Directory e -> Path Current n e -> Path Home n e
p <~/> p' = home </> p </> p'

-- | Append a file extension to a file 'Path'.
(<.>) :: Path r File e -> Component e -> Path r File e
p <.> c = FileExtension p c

-- | The current part of the path we're working with.
cur :: Path Current Directory e
cur = CurrentDirectory

-- | The root directory.
root :: Path Root Directory e
root = RootDirectory

-- | A drive letter / device name.
drive :: Component e -> Path Drive Directory e
drive = DriveName

-- | The home directory of the current user.
home :: Path Home Directory e
home = HomeDirectory

-- | The current working directory.
cwd :: Path Working Directory e
cwd = WorkingDirectory

-- | A directory.
dir :: Component e -> Path Current Directory e
dir = DirectoryPath cur

-- | A file name.
file :: Component e -> Path Current File e
file = FilePath cur
