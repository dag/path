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
import Data.Text.Encoding.Locale (encodeLocale', decodeLocale')
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.Handle (noNewlineTranslation)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

#ifdef __POSIX__
import qualified System.Posix.ByteString as ByteString
import qualified System.Posix.FilePath as ByteString
#else
import qualified System.Directory as String
import qualified System.FilePath as String
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
    show = show . decodeUtf8

-- | Segments of a path representation.
data Chunk
    = Unix !ByteString
      -- ^ Pass-through on encode; decode as POSIX path.
    | Latin !ByteString
      -- ^ Pass-through on encode; decode as latin1.
    | Bytes !ByteString
      -- ^ Pass-through on encode; decode as appropriate.
    | Unicode !Text
      -- ^ Encode as appropriate; pass-through on decode.
  deriving (Show)

-- | The singleton 'Builder' primitive.
chunk :: Chunk -> Builder
chunk c = Builder (Endo (c :))

-- | Construct a 'Builder' for a 'Unix' 'Chunk'.
unix :: ByteString -> Builder
unix = chunk . Unix

-- | Construct a 'Builder' for a 'Latin' 'Chunk'.
latin :: ByteString -> Builder
latin = chunk . Latin

-- | Construct a 'Builder' for a 'Bytes' 'Chunk'.
bytes :: ByteString -> Builder
bytes = chunk . Bytes

-- | Construct a 'Builder' for a 'Unicode' 'Chunk'.
unicode :: Text -> Builder
unicode = chunk . Unicode

-- | Construct a 'Builder' for a 'Unicode' 'Chunk' from a 'String'.
string :: String -> Builder
string = unicode . Text.pack

-- | Construct a 'Builder' from mapping a 'Name' component to a 'Chunk'.
name :: Name -> Builder
name (ByteString b) = bytes b
name (Text t) = unicode t

-- | Apply a 'Builder' to construct the final 'Chunk' list.
runBuilder :: Builder -> [Chunk]
runBuilder (Builder endo) = appEndo endo []

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
render Posix RootDirectory = latin "/"
render Windows RootDirectory = unicode "\\"
render Posix (DriveName n) = latin "/" <> name n <> latin ":" <> latin "/"
render Windows (DriveName n) = name n <> unicode ":\\"
render Posix (HostName n) = name n <> latin ":" <> latin "/"
render Windows (HostName n) = unicode "\\\\" <> name n <> unicode "\\"
render Posix HomeDirectory = latin "~" <> latin "/"
render Windows HomeDirectory = unicode "%UserProfile%\\"
render Posix WorkingDirectory = latin "." <> latin "/"
render Windows WorkingDirectory = unicode ".\\"
render Posix (DirectoryName n) = name n <> latin "/"
render Windows (DirectoryName n) = name n <> unicode "\\"
render _ (FileName n) = name n
render Posix (FileExtension n) = latin "." <> name n
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

-- * Encodings

-- | Encode the 'Unicode' chunks in a 'Builder' using the system locale.
encodeLocale :: Builder -> IO ByteString
encodeLocale builder = do
    locale <- getLocaleEncoding
    let c (Unix b) = return b
        c (Latin b) = return b
        c (Bytes b) = return b
        c (Unicode t) = encodeLocale' locale noNewlineTranslation t
    ByteString.concat <$> mapM c (runBuilder builder)

-- | Encode the 'Unicode' chunks in a 'Builder' with UTF-8.
encodeUtf8 :: Builder -> ByteString
encodeUtf8 = ByteString.concat . map c . runBuilder where
    c (Unix b) = b
    c (Latin b) = b
    c (Bytes b) = b
    c (Unicode t) = Text.encodeUtf8 t

-- | Decode the 'Bytes' chunks in a 'Builder' using the system locale.
decodeLocale :: Builder -> IO Text
decodeLocale builder = do
    locale <- getLocaleEncoding
    let c (Unix b) = decodeLocale (u b)
        c (Latin b) = return (Text.decodeLatin1 b)
        c (Bytes b) = decodeLocale' locale noNewlineTranslation b
        c (Unicode t) = return t
    Text.concat <$> mapM c (runBuilder builder)
  where
#ifdef __POSIX__
    u b
        | ByteString.isAbsolute b = latin "/" <> mconcat
            [ bytes d <> latin "/" | d <- tail (ByteString.splitDirectories b) ]
        | otherwise = mconcat
            [ bytes d <> latin "/" | d <- ByteString.splitDirectories b ]
#else
    u = bytes
#endif

-- | Decode the 'Bytes' chunks in a 'Builder' with UTF-8.
decodeUtf8 :: Builder -> Text
decodeUtf8 = Text.concat . map c . runBuilder where
    c (Unix b) = Text.decodeUtf8 b
    c (Latin b) = Text.decodeLatin1 b
    c (Bytes b) = Text.decodeUtf8 b
    c (Unicode t) = t

-- * Resolve

-- | Build an absolute path by resolving the initial 'Component' if possible.
class Resolve (a :: Component) where
    resolve :: a </> b -> IO Builder

#ifndef __WINDOWS__
instance Resolve Root where
    resolve = return . render Posix
#endif

#ifdef __WINDOWS__
instance Resolve Drive where
    resolve = return . render Windows
#endif

instance Resolve Home where
    resolve p = do
#ifdef __POSIX__
        Just homeDir <- ByteString.getEnv "HOME"
        return (unix (ByteString.addTrailingPathSeparator homeDir) <> path p)
#else
        homeDir <- String.getHomeDirectory
        return (string (String.addTrailingPathSeparator homeDir) <> path p)
#endif

instance Resolve Working where
    resolve p = do
#ifdef __POSIX__
        workingDir <- ByteString.getWorkingDirectory
        return (unix (ByteString.addTrailingPathSeparator workingDir) <> path p)
#else
        workingDir <- String.getCurrentDirectory
        return (string (String.addTrailingPathSeparator workingDir) <> path p)
#endif
