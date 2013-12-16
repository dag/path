{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: internal
-- Portability: non-portable
--
-- Unsafe and unstable internals.
module System.PathName.Internal where

import Data.ByteString (ByteString)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word8)
import System.IO (Handle, IOMode)

-- * PathName

class (Eq path, Ord path, Show path, Monoid path, IsString path) => PathName path where
    data Component path
    data Separator path
    addExtension :: path -> Component path -> path
    addTrailingPathSeparator :: path -> path
    combine :: path -> path -> path
    dropExtension :: path -> path
    dropExtensions :: path -> path
    dropFileName :: path -> path
    dropTrailingPathSeparator :: path -> path
    equalFilePath :: path -> path -> Bool
    extSeparator :: Separator path
    hasExtension :: path -> Bool
    hasTrailingPathSeparator :: path -> Bool
    isAbsolute :: path -> Bool
    isExtSeparator :: Separator path -> Bool
    isPathSeparator :: Separator path -> Bool
    isRelative :: path -> Bool
    isSearchPathSeparator :: Separator path -> Bool
    isValid :: path -> Bool
    joinPath :: [path] -> path
    makeRelative :: path -> path -> path
    makeValid :: path -> path
    normalise :: path -> path
    pathSeparator :: Separator path
    pathSeparators :: [Separator path]
    replaceBaseName :: path -> Component path -> path
    replaceDirectory :: path -> Component path -> path
    replaceExtension :: path -> Component path -> path
    replaceFileName :: path -> Component path -> path
    searchPathSeparator :: Separator path
    splitDirectories :: path -> [path]
    splitExtension :: path -> (Component path, Component path)
    splitExtensions :: path -> (path, Component path)
    splitFileName :: path -> (Component path, Component path)
    splitPath :: path -> [path]
    splitSearchPath :: Component path -> [path]
    takeBaseName :: path -> Component path
    takeDirectory :: path -> path
    takeExtension :: path -> Component path
    takeExtensions :: path -> Component path
    takeFileName :: path -> path

-- * PosixPath

newtype PosixPath = PosixPath ByteString deriving (Eq, Ord, Show, IsString)

instance Monoid PosixPath where
    mempty = PosixPath mempty
    mappend = combine
    mconcat = joinPath

instance PathName PosixPath where
    newtype Component PosixPath = PosixComponent ByteString
    newtype Separator PosixPath = PosixSeparator Word8

-- * WindowsPath

newtype WindowsPath = WindowsPath Text deriving (Eq, Ord, Show, IsString)

instance Monoid WindowsPath where
    mempty = WindowsPath mempty
    mappend = combine
    mconcat = joinPath

instance PathName WindowsPath where
    newtype Component WindowsPath = WindowsComponent Text
    newtype Separator WindowsPath = WindowsSeparator Char

dropDrive :: WindowsPath -> WindowsPath
dropDrive = error "System.PathName.dropDrive: not implemented"

hasDrive :: WindowsPath -> Bool
hasDrive = error "System.PathName.hasDrive: not implemented"

isDrive :: WindowsPath -> Bool
isDrive = error "System.PathName.isDrive: not implemented"

joinDrive :: WindowsPath -> WindowsPath -> WindowsPath
joinDrive = error "System.PathName.joinDrive: not implemented"

splitDrive :: WindowsPath -> (WindowsPath, WindowsPath)
splitDrive = error "System.PathName.splitDrive: not implemented"

takeDrive :: WindowsPath -> WindowsPath
takeDrive = error "System.PathName.takeDrive: not implemented"

-- * Permissions

data Permissions = Permissions { readable, writable, executable, searchable :: Bool }

emptyPermissions :: Permissions
emptyPermissions = error "System.PathName.emptyPermissions: not implemented"

setOwnerExecutable :: Bool -> Permissions -> Permissions
setOwnerExecutable = error "System.PathName.setOwnerExecutable: not implemented"

setOwnerReadable :: Bool -> Permissions -> Permissions
setOwnerReadable = error "System.PathName.setOwnerReadable: not implemented"

setOwnerSearchable :: Bool -> Permissions -> Permissions
setOwnerSearchable = error "System.PathName.setOwnerSearchable: not implemented"

setOwnerWritable :: Bool -> Permissions -> Permissions
setOwnerWritable = error "System.PathName.setOwnerWritable: not implemented"

-- * SystemPath

type SystemPath =
#ifdef __POSIX__
    PosixPath
#else
    WindowsPath
#endif

appendFile :: SystemPath -> String -> IO ()
appendFile = error "System.PathName.appendFile: not implemented"

canonicalizePath :: SystemPath -> IO SystemPath
canonicalizePath = error "System.PathName.canonicalizePath: not implemented"

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

findExecutable :: Component SystemPath -> IO (Maybe SystemPath)
findExecutable = error "System.PathName.findExecutable: not implemented"

findFile :: [SystemPath] -> Component SystemPath -> IO (Maybe SystemPath)
findFile = error "System.PathName.findFile: not implemented"

getAppUserDataDirectory :: Component SystemPath -> IO SystemPath
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

makeRelativeToCurrentDirectory :: SystemPath -> IO SystemPath
makeRelativeToCurrentDirectory = error "System.PathName.makeRelativeToCurrentDirectory: not implemented"

openBinaryFile :: SystemPath -> IOMode -> IO Handle
openBinaryFile = error "System.PathName.openBinaryFile: not implemented"

openBinaryTempFile :: SystemPath -> Component SystemPath -> IO (SystemPath, Handle)
openBinaryTempFile = error "System.PathName.openBinaryTempFile: not implemented"

openBinaryTempFileWithDefaultPermissions :: SystemPath -> Component SystemPath -> IO (SystemPath, Handle)
openBinaryTempFileWithDefaultPermissions = error "System.PathName.openBinaryTempFileWithDefaultPermissions: not implemented"

openFile :: SystemPath -> IOMode -> IO Handle
openFile = error "System.PathName.openFile: not implemented"

openTempFile :: SystemPath -> Component SystemPath -> IO (SystemPath, Handle)
openTempFile = error "System.PathName.openTempFile: not implemented"

openTempFileWithDefaultPermissions :: SystemPath -> Component SystemPath -> IO (SystemPath, Handle)
openTempFileWithDefaultPermissions = error "System.PathName.openTempFileWithDefaultPermissions: not implemented"

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

setCurrentDirectory :: SystemPath -> IO ()
setCurrentDirectory = error "System.PathName.setCurrentDirectory: not implemented"

setPermissions :: SystemPath -> Permissions -> IO ()
setPermissions = error "System.PathName.setPermissions: not implemented"

withBinaryFile :: SystemPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = error "System.PathName.withBinaryFile: not implemented"

withFile :: SystemPath -> IOMode -> (Handle -> IO r) -> IO r
withFile = error "System.PathName.withFile: not implemented"

writeFile :: SystemPath -> String -> IO ()
writeFile = error "System.PathName.writeFile: not implemented"
