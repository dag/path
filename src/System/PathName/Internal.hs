{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Unsafe #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: internal
-- Portability: non-portable
--
-- Unsafe and unstable internals.
module System.PathName.Internal where

import Prelude hiding (appendFile, readFile, writeFile)

import Data.ByteString (ByteString)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word8)
import GHC.TypeLits (Sing)
import System.IO (Handle, IOMode)

-- * Permissions

data Permissions

-- * Interface

data Interface = Posix | Windows

type System =
#ifdef __POSIX__
    Posix
#else
    Windows
#endif

data instance Sing (k :: Interface) where
    SPosix :: Sing Posix
    SWindows :: Sing Windows

posix :: Sing Posix
posix = SPosix

windows :: Sing Windows
windows = SWindows

-- * Component

data Component :: Interface -> * where
    PosixComponent :: !ByteString -> Component Posix
    WindowsComponent :: !Text -> Component Windows

deriving instance Eq (Component iface)

deriving instance Ord (Component iface)

deriving instance Show (Component iface)

instance IsString PosixComponent where
    fromString = PosixComponent . fromString

instance IsString WindowsComponent where
    fromString = WindowsComponent . fromString

type PosixComponent = Component Posix
type WindowsComponent = Component Windows
type SystemComponent = Component System

-- * Separator

data Separator :: Interface -> * where
    PosixSeparator :: !Word8 -> Separator Posix
    WindowsSeparator :: !Char -> Separator Windows

deriving instance Eq (Separator iface)

deriving instance Ord (Separator iface)

deriving instance Show (Separator iface)

type PosixSeparator = Separator Posix
type WindowsSeparator = Separator Windows
type SystemSeparator = Separator System

-- * PathName

data PathName :: Interface -> * where
    PosixPath :: !ByteString -> PathName Posix
    WindowsPath :: !Text -> PathName Windows

deriving instance Eq (PathName iface)

deriving instance Ord (PathName iface)

deriving instance Show (PathName iface)

instance Monoid PosixPath where
    mempty = PosixPath mempty
    mappend = combine
    mconcat = joinPath

instance Monoid WindowsPath where
    mempty = WindowsPath mempty
    mappend = combine
    mconcat = joinPath

instance IsString PosixPath where
    fromString = PosixPath . fromString

instance IsString WindowsPath where
    fromString = WindowsPath . fromString

type PosixPath = PathName Posix
type WindowsPath = PathName Windows
type SystemPath = PathName System

addExtension :: PathName iface -> Component iface -> PathName iface
addExtension = error "System.PathName.addExtension: not implemented"

addTrailingPathSeparator :: PathName iface -> PathName iface
addTrailingPathSeparator = error "System.PathName.addTrailingPathSeparator: not implemented"

appendFile :: SystemPath -> String -> IO ()
appendFile = error "System.PathName.appendFile: not implemented"

canonicalizePath :: SystemPath -> IO SystemPath
canonicalizePath = error "System.PathName.canonicalizePath: not implemented"

combine :: PathName iface -> PathName iface -> PathName iface
combine = error "System.PathName.combine: not implemented"

copyFile :: SystemPath -> SystemPath -> IO ()
copyFile = error "System.PathName.copyFile: not implemented"

copyPermissions :: SystemPath -> SystemPath -> IO ()
copyPermissions = error "System.PathName.copyPermissions: not implemented"

createDirectory :: SystemPath -> IO ()
createDirectory = error "System.PathName.createDirectory: not implemented"

createDirectoryIfMissing :: Bool -> SystemPath -> IO ()
createDirectoryIfMissing = error "System.PathName.createDirectoryIfMissing: not implemented"

doesDirectoryExist :: SystemPath -> IO Bool
doesDirectoryExist = error "System.PathName.doesDirectoryExist: not implemented"

doesFileExist :: SystemPath -> IO Bool
doesFileExist = error "System.PathName.doesFileExist: not implemented"

dropDrive :: WindowsPath -> WindowsPath
dropDrive = error "System.PathName.dropDrive: not implemented"

dropExtension :: PathName iface -> PathName iface
dropExtension = error "System.PathName.dropExtension: not implemented"

dropExtensions :: PathName iface -> PathName iface
dropExtensions = error "System.PathName.dropExtensions: not implemented"

dropFileName :: PathName iface -> PathName iface
dropFileName = error "System.PathName.dropFileName: not implemented"

dropTrailingPathSeparator :: PathName iface -> PathName iface
dropTrailingPathSeparator = error "System.PathName.dropTrailingPathSeparator: not implemented"

emptyPermissions :: Permissions
emptyPermissions = error "System.PathName.emptyPermissions: not implemented"

equalFilePath :: PathName iface -> PathName iface -> Bool
equalFilePath = error "System.PathName.equalFilePath: not implemented"

extSeparator :: Sing iface -> Separator iface
extSeparator SPosix = (PosixSeparator . toEnum . fromEnum) '.'
expSeparator SWindows = WindowsSeparator '.'

findExecutable :: Component System -> IO (Maybe SystemPath)
findExecutable = error "System.PathName.findExecutable: not implemented"

findFile :: [SystemPath] -> Component System -> IO (Maybe SystemPath)
findFile = error "System.PathName.findFile: not implemented"

getAppUserDataDirectory :: Component System -> IO SystemPath
getAppUserDataDirectory = error "System.PathName.getAppUserDataDirectory: not implemented"

getCurrentDirectory :: IO SystemPath
getCurrentDirectory = error "System.PathName.getCurrentDirectory: not implemented"

getDirectoryContents :: SystemPath -> IO [SystemPath]
getDirectoryContents = error "System.PathName.getDirectoryContents: not implemented"

getHomeDirectory :: IO SystemPath
getHomeDirectory = error "System.PathName.getHomeDirectory: not implemented"

getModificationTime :: SystemPath -> IO UTCTime
getModificationTime = error "System.PathName.getModificationTime: not implemented"

getPermissions :: SystemPath -> IO Permissions
getPermissions = error "System.PathName.getPermissions: not implemented"

getSearchPath :: IO [SystemPath]
getSearchPath = error "System.PathName.getSearchPath: not implemented"

getTemporaryDirectory :: IO SystemPath
getTemporaryDirectory = error "System.PathName.getTemporaryDirectory: not implemented"

getUserDocumentsDirectory :: IO SystemPath
getUserDocumentsDirectory = error "System.PathName.getUserDocumentsDirectory: not implemented"

hasDrive :: WindowsPath -> Bool
hasDrive = error "System.PathName.hasDrive: not implemented"

hasExtension :: PathName iface -> Bool
hasExtension = error "System.PathName.hasExtension: not implemented"

hasTrailingPathSeparator :: PathName iface -> Bool
hasTrailingPathSeparator = error "System.PathName.hasTrailingPathSeparator: not implemented"

isAbsolute :: PathName iface -> Bool
isAbsolute = error "System.PathName.isAbsolute: not implemented"

isDrive :: WindowsPath -> Bool
isDrive = error "System.PathName.isDrive: not implemented"

isExtSeparator :: Separator iface -> Bool
isExtSeparator s@(PosixSeparator _) = s == extSeparator posix
isExtSeparator s@(WindowsSeparator _) = s == extSeparator windows

isPathSeparator :: Separator iface -> Bool
isPathSeparator s@(PosixSeparator _) = s == pathSeparator posix
isPathSeparator s@(WindowsSeparator _) = s `elem` pathSeparators windows

isRelative :: PathName iface -> Bool
isRelative = error "System.PathName.isRelative: not implemented"

isSearchPathSeparator :: Separator iface -> Bool
isSearchPathSeparator s@(PosixSeparator _) = s == searchPathSeparator posix
isSearchPathSeparator s@(WindowsSeparator _) = s == searchPathSeparator windows

isValid :: PathName iface -> Bool
isValid = error "System.PathName.isValid: not implemented"

joinDrive :: WindowsPath -> WindowsPath -> WindowsPath
joinDrive = error "System.PathName.joinDrive: not implemented"

joinPath :: [PathName iface] -> PathName iface
joinPath = error "System.PathName.joinPath: not implemented"

makeRelative :: PathName iface -> PathName iface -> PathName iface
makeRelative = error "System.PathName.makeRelative: not implemented"

makeRelativeToCurrentDirectory :: SystemPath -> IO SystemPath
makeRelativeToCurrentDirectory = error "System.PathName.makeRelativeToCurrentDirectory: not implemented"

makeValid :: PathName iface -> PathName iface
makeValid = error "System.PathName.makeValid: not implemented"

normalise :: PathName iface -> PathName iface
normalise = error "System.PathName.normalise: not implemented"

openBinaryFile :: SystemPath -> IOMode -> IO Handle
openBinaryFile = error "System.PathName.openBinaryFile: not implemented"

openBinaryTempFile :: SystemPath -> Component System -> IO (SystemPath, Handle)
openBinaryTempFile = error "System.PathName.openBinaryTempFile: not implemented"

openBinaryTempFileWithDefaultPermissions :: SystemPath -> Component System -> IO (SystemPath, Handle)
openBinaryTempFileWithDefaultPermissions = error "System.PathName.openBinaryTempFileWithDefaultPermissions: not implemented"

openFile :: SystemPath -> IOMode -> IO Handle
openFile = error "System.PathName.openFile: not implemented"

openTempFile :: SystemPath -> Component System -> IO (SystemPath, Handle)
openTempFile = error "System.PathName.openTempFile: not implemented"

openTempFileWithDefaultPermissions :: SystemPath -> Component System -> IO (SystemPath, Handle)
openTempFileWithDefaultPermissions = error "System.PathName.openTempFileWithDefaultPermissions: not implemented"

pathSeparator :: Sing iface -> Separator iface
pathSeparator SPosix = (PosixSeparator . toEnum . fromEnum) '/'
pathSeparator SWindows = WindowsSeparator '\\'

pathSeparators :: Sing iface -> [Separator iface]
pathSeparators SPosix = [pathSeparator posix]
pathSeparators SWindows = [pathSeparator windows, WindowsSeparator '/']

readFile :: SystemPath -> IO String
readFile = error "System.PathName.readFile: not implemented"

removeDirectory :: SystemPath -> IO ()
removeDirectory = error "System.PathName.removeDirectory: not implemented"

removeDirectoryRecursive :: SystemPath -> IO ()
removeDirectoryRecursive = error "System.PathName.removeDirectoryRecursive: not implemented"

removeFile :: SystemPath -> IO ()
removeFile = error "System.PathName.removeFile: not implemented"

renameDirectory :: SystemPath -> SystemPath -> IO ()
renameDirectory = error "System.PathName.renameDirectory: not implemented"

renameFile :: SystemPath -> SystemPath -> IO ()
renameFile = error "System.PathName.renameFile: not implemented"

replaceBaseName :: PathName iface -> Component iface -> PathName iface
replaceBaseName = error "System.PathName.replaceBaseName: not implemented"

replaceDirectory :: PathName iface -> Component iface -> PathName iface
replaceDirectory = error "System.PathName.replaceDirectory: not implemented"

replaceExtension :: PathName iface -> Component iface -> PathName iface
replaceExtension = error "System.PathName.replaceExtension: not implemented"

replaceFileName :: PathName iface -> Component iface -> PathName iface
replaceFileName = error "System.PathName.replaceFileName: not implemented"

searchPathSeparator :: Sing iface -> Separator iface
searchPathSeparator SPosix = (PosixSeparator . toEnum . fromEnum) ';'
searchPathSeparator SWindows = WindowsSeparator ';'

setCurrentDirectory :: SystemPath -> IO ()
setCurrentDirectory = error "System.PathName.setCurrentDirectory: not implemented"

setOwnerExecutable :: Bool -> Permissions -> Permissions
setOwnerExecutable = error "System.PathName.setOwnerExecutable: not implemented"

setOwnerReadable :: Bool -> Permissions -> Permissions
setOwnerReadable = error "System.PathName.setOwnerReadable: not implemented"

setOwnerSearchable :: Bool -> Permissions -> Permissions
setOwnerSearchable = error "System.PathName.setOwnerSearchable: not implemented"

setOwnerWritable :: Bool -> Permissions -> Permissions
setOwnerWritable = error "System.PathName.setOwnerWritable: not implemented"

setPermissions :: SystemPath -> Permissions -> IO ()
setPermissions = error "System.PathName.setPermissions: not implemented"

splitDirectories :: PathName iface -> [PathName iface]
splitDirectories = error "System.PathName.splitDirectories: not implemented"

splitDrive :: WindowsPath -> (WindowsPath, WindowsPath)
splitDrive = error "System.PathName.splitDrive: not implemented"

splitExtension :: PathName iface -> (Component iface, Component iface)
splitExtension = error "System.PathName.splitExtension: not implemented"

splitExtensions :: PathName iface -> (PathName iface, Component iface)
splitExtensions = error "System.PathName.splitExtensions: not implemented"

splitFileName :: PathName iface -> (Component iface, Component iface)
splitFileName = error "System.PathName.splitFileName: not implemented"

splitPath :: PathName iface -> [PathName iface]
splitPath = error "System.PathName.splitPath: not implemented"

splitSearchPath :: Component iface -> [PathName iface]
splitSearchPath = error "System.PathName.splitSearchPath: not implemented"

takeBaseName :: PathName iface -> Component iface
takeBaseName = error "System.PathName.takeBaseName: not implemented"

takeDirectory :: PathName iface -> PathName iface
takeDirectory = error "System.PathName.takeDirectory: not implemented"

takeDrive :: WindowsPath -> WindowsPath
takeDrive = error "System.PathName.takeDrive: not implemented"

takeExtension :: PathName iface -> Component iface
takeExtension = error "System.PathName.takeExtension: not implemented"

takeExtensions :: PathName iface -> Component iface
takeExtensions = error "System.PathName.takeExtensions: not implemented"

takeFileName :: PathName iface -> PathName iface
takeFileName = error "System.PathName.takeFileName: not implemented"

withBinaryFile :: SystemPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = error "System.PathName.withBinaryFile: not implemented"

withFile :: SystemPath -> IOMode -> (Handle -> IO r) -> IO r
withFile = error "System.PathName.withFile: not implemented"

writeFile :: SystemPath -> String -> IO ()
writeFile = error "System.PathName.writeFile: not implemented"
