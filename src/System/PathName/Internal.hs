{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: internal
-- Portability: non-portable
--
-- Unsafe and unstable internals.
module System.PathName.Internal where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Monoid (Monoid(..), (<>))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.Handle (noNewlineTranslation)
import qualified Data.Text as Text
import qualified Data.Text.Encoding.Locale as Text

#ifdef __POSIX__
import qualified Data.Text.Encoding as Text
import qualified Data.Time.Clock.POSIX as Backend
import qualified System.IO.Error as IO
import qualified System.Posix.ByteString as Backend
import qualified System.Posix.Directory.Traversals as Backend
import qualified System.Posix.FilePath as Backend
#else
import qualified Data.ByteString.Char8 as ByteString
import qualified System.Directory as Backend
import qualified System.FilePath as Backend
#endif

{- The way we use unsafeCoerce here is OK, but it's very easy to get wrong when
 - every little mistake will type check without complaint.  On GHC 7.8 we
 - should use Coercible instead, and then if everything type checks using safe
 - coercions, hopefully it's an indicator that the unsafe coercions (which
 - we'll still need on GHC 7.6) are good too.  Fingers crossed.
 -}

import Unsafe.Coerce (unsafeCoerce)

-- * Backend

{- It would probably be better to make PathName always wrap a ByteString, and
 - implement the pure functions directly, and then simply decode when necessary
 - for IO operations.  This would enable us to export functions between
 - ByteString and PathName, make the Show instance consistent across platforms,
 - and perhaps even permit an IsString instance (debatable; same issues as the
 - Char8 APIs in bytestring).
 -
 - However, this would more or less require re-implementing (and maintaining)
 - the whole filepath package using bytestrings, and we can't simply copy the
 - code from the posix-paths package since it doesn't handle Windows paths at
 - all.
 -
 - It's tempting to do something isomorphic to:
 -
 -     type Component = Either Text ByteString
 -     type PathName = ([Component], [Component])
 -
 - However, that's pretty much exactly what System.Path is doing.  The
 - motivation for the PathName type is primarily to be a fast and monomorphic
 - cross-platform representation of paths, interfacing the high-level Path type
 - with the low-level filesystem.
 -}

-- | The backend path type.
type Backend =
#ifdef __POSIX__
    Backend.RawFilePath
#else
    Backend.FilePath
#endif

-- * PathName

-- | A path on the local filesystem.
newtype PathName = PathName { unPathName :: Backend } deriving (Eq, Ord)

instance Show PathName where
    showsPrec d x = showsPrec d (unPathName x)

instance Monoid PathName where
    mempty = PathName mempty
    mappend = combine
    mconcat = joinPath

-- | Smart 'PathName' constructor.
pathName :: Backend -> PathName
pathName = unsafeCoerce dropTrailingPathSeparator

-- * Extension

-- | Filename extension monoid.  Unlike 'addExtension', 'mempty' is the
-- identity.
newtype FileName = FileName { getFileName :: PathName }
  deriving (Eq, Ord, Show)

instance Monoid FileName where
    mempty = FileName mempty
    mappend a b
        | a == mempty = b
        | b == mempty = a
        | otherwise = unsafeCoerce addExtension a b

-- * Encoding

-- | Build a 'PathName' by processing components with the system locale
-- encoding, and then concatenating them with 'Monoid'.
--
-- >>> encode [Left "/", Right "etc"] [Left "passwd", Right "gz"]
-- "/etc/passwd.gz"
build
    :: [Either ByteString Text]  -- ^ 'PathName' components
    -> [Either ByteString Text]  -- ^ 'FileName' components
    -> IO PathName
build ps fs = do
    encoding <- getLocaleEncoding
#ifdef __POSIX__
    let d = return . unsafeCoerce
        e = unsafeCoerce Text.encodeLocale' encoding nl
#else
    let d = fmap (unsafeCoerce . Text.unpack) . Text.decodeLocale' encoding nl
        e = return . unsafeCoerce Text.unpack
#endif
    path <- mconcat <$> mapM (either d e) ps
    file <- mconcat . unsafeCoerce <$> mapM (either d e) fs
    return $ dropTrailingPathSeparator (path <> getFileName file)
  where
    nl = noNewlineTranslation

-- | Decode (if necessary) a 'PathName' to 'Text' using the system locale
-- encoding.
decode :: PathName -> IO Text
#ifdef __POSIX__
decode path = do
    encoding <- getLocaleEncoding
    let ps = splitDirectories path
        nl = noNewlineTranslation
        d t | bs <- unsafeCoerce t, bs `elem` ["/", "."] =
                return . Text.decodeLatin1 $ bs
            | otherwise = unsafeCoerce Text.decodeLocale' encoding nl t
    ts <- Text.intercalate "/" <$> mapM d ps
    if isAbsolute path then
        return . Text.tail $ ts
    else
        return ts
#else
decode = return . unsafeCoerce Text.pack
#endif

-- | Encode (if necessary) a 'PathName' to a 'ByteString' using the system
-- locale encoding.
encode :: PathName -> IO ByteString
encode path = do
#ifdef __POSIX__
    return . unsafeCoerce $ path
#else
    encoding <- getLocaleEncoding
    let nl = noNewlineTranslation
        e b | ts <- unsafeCoerce b, ts `elem` ["/", "."] =
                return . ByteString.pack . Text.unpack $ ts
            | otherwise =
                unsafeCoerce (Text.encodeLocale' encoding nl . Text.pack) b
#ifdef __WINDOWS__
    e path
#else
    let ps = splitDirectories path
    bs <- ByteString.intercalate "/" <$> mapM e ps
    if isAbsolute path then
        return . ByteString.tail $ bs
    else
        return bs
#endif
#endif

-- * Path manipulation

-- (<.>) :: PathName -> PathName -> PathName
-- (</>) :: PathName -> PathName -> PathName

-- | 'Backend.addExtension'
addExtension :: PathName -> PathName -> PathName
addExtension = unsafeCoerce Backend.addExtension

-- | 'Backend.addTrailingPathSeparator'
addTrailingPathSeparator :: PathName -> PathName
addTrailingPathSeparator = unsafeCoerce Backend.addTrailingPathSeparator

-- | 'Backend.combine'
combine :: PathName -> PathName -> PathName
combine = unsafeCoerce Backend.combine

-- dropDrive :: PathName -> PathName

-- | 'Backend.dropExtension'
dropExtension :: PathName -> PathName
dropExtension = unsafeCoerce Backend.dropExtension

-- | 'Backend.dropExtensions'
dropExtensions :: PathName -> PathName
dropExtensions = unsafeCoerce Backend.dropExtensions

-- | 'Backend.dropFileName'
dropFileName :: PathName -> PathName
dropFileName = unsafeCoerce Backend.dropFileName

-- | 'Backend.dropTrailingPathSeparator'
dropTrailingPathSeparator :: PathName -> PathName
dropTrailingPathSeparator = unsafeCoerce Backend.dropTrailingPathSeparator

-- equalPathName :: PathName -> PathName -> Bool
-- extSeparator :: Char
-- getSearchPath :: IO [PathName]
-- hasDrive :: PathName -> Bool

-- | 'Backend.hasExtension'
hasExtension :: PathName -> Bool
hasExtension = unsafeCoerce Backend.hasExtension

-- | 'Backend.hasTrailingPathSeparator'
hasTrailingPathSeparator :: PathName -> Bool
hasTrailingPathSeparator = unsafeCoerce Backend.hasTrailingPathSeparator

-- | 'Backend.isAbsolute'
isAbsolute :: PathName -> Bool
isAbsolute = unsafeCoerce Backend.isAbsolute

-- isDrive :: PathName -> Bool
-- isExtSeparator :: Char -> Bool
-- isPathSeparator :: Char -> Bool

-- | 'Backend.isRelative'
isRelative :: PathName -> Bool
isRelative = unsafeCoerce Backend.isRelative

-- isSearchPathSeparator :: Char -> Bool
-- isValid :: PathName -> Bool
-- joinDrive :: PathName -> PathName -> PathName

-- | 'Backend.joinPath'
joinPath :: [PathName] -> PathName
joinPath = unsafeCoerce Backend.joinPath

-- makeRelative :: PathName -> PathName -> PathName
-- makeValid :: PathName -> PathName
-- normalise :: PathName -> PathName
-- pathSeparator :: Char
-- pathSeparators :: [Char]

-- | 'Backend.replaceBaseName'
replaceBaseName :: PathName -> PathName -> PathName
replaceBaseName = unsafeCoerce Backend.replaceBaseName

-- | 'Backend.replaceDirectory'
replaceDirectory :: PathName -> PathName -> PathName
replaceDirectory = unsafeCoerce Backend.replaceDirectory

-- | 'Backend.replaceExtension'
replaceExtension :: PathName -> PathName -> PathName
replaceExtension = unsafeCoerce Backend.replaceExtension

-- | 'Backend.replaceFileName'
replaceFileName :: PathName -> PathName -> PathName
replaceFileName = unsafeCoerce Backend.replaceFileName

-- searchPathSeparator :: Char

-- | 'Backend.splitDirectories'
splitDirectories :: PathName -> [PathName]
splitDirectories = unsafeCoerce Backend.splitDirectories

-- splitDrive :: PathName -> (PathName, PathName)

-- | 'Backend.splitExtension'
splitExtension :: PathName -> (PathName, PathName)
splitExtension = unsafeCoerce Backend.splitExtension

-- | 'Backend.splitExtensions'
splitExtensions :: PathName -> (PathName, PathName)
splitExtensions = unsafeCoerce Backend.splitExtensions

-- | 'Backend.splitFileName'
splitFileName :: PathName -> (PathName, PathName)
splitFileName = unsafeCoerce Backend.splitFileName

-- | 'Backend.splitPath'
splitPath :: PathName -> [PathName]
splitPath = unsafeCoerce Backend.splitPath

-- splitSearchPath :: PathName -> [PathName]

-- | 'Backend.takeBaseName'
takeBaseName :: PathName -> PathName
takeBaseName = unsafeCoerce Backend.takeBaseName

-- | 'Backend.takeDirectory'
takeDirectory :: PathName -> PathName
takeDirectory = unsafeCoerce Backend.takeDirectory

-- takeDrive :: PathName -> PathName

-- | 'Backend.takeExtension'
takeExtension :: PathName -> PathName
takeExtension = unsafeCoerce Backend.takeExtension

-- | 'Backend.takeExtensions'
takeExtensions :: PathName -> PathName
takeExtensions = unsafeCoerce Backend.takeExtensions

-- | 'Backend.takeFileName'
takeFileName :: PathName -> PathName
takeFileName = unsafeCoerce Backend.takeFileName

-- * Lenses

{- Taken more or less verbatim from the lens package, which is under the same
 - license as this package (BSD3).  (C) 2012-13 Edward Kmett.
 -}

-- (<.>=) :: MonadState s m => ASetter' s PathName -> String -> m ()
-- (<.>~) :: ASetter s a PathName PathName -> String -> s -> a
-- (</>=) :: MonadState s m => ASetter' s PathName -> PathName -> m ()
-- (</>~) :: ASetter s t PathName PathName -> PathName -> s -> t
-- (<<.>=) :: MonadState s m => LensLike' ((,) PathName) s PathName -> String -> m PathName
-- (<<.>~) :: LensLike ((,) PathName) s a PathName PathName -> String -> s -> (PathName, a)
-- (<</>=) :: MonadState s m => LensLike' ((,) PathName) s PathName -> PathName -> m PathName
-- (<</>~) :: LensLike ((,) PathName) s a PathName PathName -> PathName -> s -> (PathName, a)

-- | @'basename' :: Lens' 'PathName' 'PathName'@
basename :: (Functor f) => (PathName -> f PathName) -> PathName -> f PathName
basename f p = (`addExtension` takeExtension p) . (takeDirectory p `mappend`)
    <$> f (takeBaseName p)

-- | @'directory' :: Lens' 'PathName' 'PathName'@
directory :: (Functor f) => (PathName -> f PathName) -> PathName -> f PathName
directory f p = (`combine` takeFileName p) <$> f (takeDirectory p)

-- | @'extension' :: Lens' 'PathName' 'PathName'@
extension :: (Functor f) => (PathName -> f PathName) -> PathName -> f PathName
extension f p = addExtension n <$> f e where (n, e) = splitExtension p

-- | @'filename' :: Lens' 'PathName' 'PathName'@
filename :: (Functor f) => (PathName -> f PathName) -> PathName -> f PathName
filename f p = (takeDirectory p `combine`) <$> f (takeFileName p)

-- * IO operations

{- We should probably take greater care to ensure consistent exceptions across
 - platforms here, or at least document what additional exceptions might get
 - thrown by the POSIX backend, that aren't thrown by the directory package.
 -
 - Also, we should consider making directory an unconditional dependency and
 - use its Permissions type rather than re-defining it here.  On the other
 - hand, that might be questionable if PathName ever moves to ByteString
 - unconditionally.
 -}

-- data Permissions = Permissions
--     { readable, writable, executable, searchable :: Bool }
--   deriving (Eq, Ord, Read, Show)

canonicalizePath :: PathName -> IO PathName
#ifdef __POSIX__
-- ^ 'Backend.realpath'
canonicalizePath = unsafeCoerce Backend.realpath
#else
-- ^ 'Backend.canonicalizePath'
canonicalizePath = unsafeCoerce Backend.canonicalizePath
#endif

-- copyFile :: PathName -> PathName -> IO ()
-- copyPermissions :: PathName -> PathName -> IO ()

-- | 'Backend.createDirectory'
createDirectory :: PathName -> IO ()
createDirectory = unsafeCoerce
#ifdef __POSIX__
    (`Backend.createDirectory` Backend.accessModes)
#else
    Backend.createDirectory
#endif

-- createDirectoryIfMissing :: Bool -> PathName -> IO ()
-- doesDirectoryExist :: PathName -> IO Bool
-- doesFileExist :: PathName -> IO Bool
-- emptyPermissions :: Permissions
-- findExecutable :: String -> IO (Maybe PathName)
-- findFile :: [PathName] -> String -> IO (Maybe PathName)
-- getAppUserDataDirectory :: String -> IO PathName

getCurrentDirectory :: IO PathName
#ifdef __POSIX__
-- ^ 'Backend.getWorkingDirectory'
getCurrentDirectory = unsafeCoerce Backend.getWorkingDirectory
#else
-- ^ 'Backend.getCurrentDirectory'
getCurrentDirectory = unsafeCoerce Backend.getCurrentDirectory
#endif

-- getDirectoryContents :: PathName -> IO [PathName]

getHomeDirectory :: IO PathName
#ifdef __POSIX__
-- ^ 'Backend.getEnv' @\"HOME\"@
getHomeDirectory =
    maybe (ioError ioeHOME) (return . unsafeCoerce) =<< Backend.getEnv "HOME"
  where
    ioeHOME = IO.mkIOError IO.doesNotExistErrorType "getHomeDirectory" Nothing
        (Just "HOME") `IO.ioeSetErrorString` "no environment variable"
#else
-- ^ 'Backend.getHomeDirectory'
getHomeDirectory = unsafeCoerce Backend.getHomeDirectory
#endif

getModificationTime :: PathName -> IO UTCTime
#ifdef __POSIX__
-- ^ 'Backend.modificationTimeHiRes'
getModificationTime =
    fmap (toUTCTime . modificationTime) . unsafeCoerce getFileStatus
  where
    toUTCTime = Backend.posixSecondsToUTCTime
    modificationTime = Backend.modificationTimeHiRes
    getFileStatus = Backend.getFileStatus
#else
-- ^ 'Backend.getModificationTime'
getModificationTime = unsafeCoerce Backend.getModificationTime
#endif

-- getPermissions :: PathName -> IO Permissions
-- getTemporaryDirectory :: IO PathName
-- getUserDocumentsDirectory :: IO PathName
-- makeRelativeToCurrentDirectory :: PathName -> IO PathName

-- | 'Backend.removeDirectory'
removeDirectory :: PathName -> IO ()
removeDirectory = unsafeCoerce Backend.removeDirectory

-- removeDirectoryRecursive :: PathName -> IO ()

removeFile :: PathName -> IO ()
#ifdef __POSIX__
-- ^ 'Backend.removeLink'
removeFile = unsafeCoerce Backend.removeLink
#else
-- ^ 'Backend.removeFile'
removeFile = unsafeCoerce Backend.removeFile
#endif

-- renameDirectory :: PathName -> PathName -> IO ()
-- renameFile :: PathName -> PathName -> IO ()
-- setCurrentDirectory :: PathName -> IO ()
-- setOwnerExecutable :: Bool -> Permissions -> Permissions
-- setOwnerReadable :: Bool -> Permissions -> Permissions
-- setOwnerSearchable :: Bool -> Permissions -> Permissions
-- setOwnerWritable :: Bool -> Permissions -> Permissions
-- setPermissions :: PathName -> Permissions -> IO ()

-- * Handle operations

-- appendFile :: PathName -> String -> IO ()
-- openBinaryFile :: PathName -> IOMode -> IO Handle
-- openBinaryTempFile :: PathName -> String -> IO (PathName, Handle)
-- openBinaryTempFileWithDefaultPermissions :: PathName -> String -> IO (PathName, Handle)
-- openFile :: PathName -> IOMode -> IO Handle
-- openTempFile :: PathName -> String -> IO (PathName, Handle)
-- openTempFileWithDefaultPermissions :: PathName -> String -> IO (PathName, Handle)
-- readFile :: PathName -> IO String
-- withBinaryFile :: PathName -> IOMode -> (Handle -> IO r) -> IO r
-- withFile :: PathName -> IOMode -> (Handle -> IO r) -> IO r
-- writeFile :: PathName -> String -> IO ()
