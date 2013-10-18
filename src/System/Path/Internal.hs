{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Unsafe #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: internal
-- Portability: non-portable
--
-- Unsafe and unstable internals.
module System.Path.Internal where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Monoid (Monoid(..), Endo(..), (<>))
import Data.String (IsString(..))
import Data.Text (Text)
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.Handle (noNewlineTranslation)
import qualified Data.Text.Encoding.Locale as Text
import qualified System.PathName as PathName
import System.PathName.Internal (PathName(..), pathName)

#ifdef __POSIX__
import qualified Data.ByteString as ByteString
#else
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
#endif

-- * Mono

-- | Construct a monomorphic representation of a type.
class Mono t where
    type Monomorphic t
    mono :: t -> Monomorphic t

-- * Name

-- | A named component of a path, to be encoded or decoded as appropriate.
data Name = ByteString !ByteString | Text !Text deriving (Eq, Ord, Show)

instance IsString Name where
    fromString = Text . fromString

-- * Vertex

-- | The kind of types of vertices in the filesystem graph, and the objects of
-- the 'Path' category.
data Vertex = Root | Drive | Remote | Home | Working | Directory | File
  deriving (Eq, Ord, Show)

-- | Test if a 'Vertex' is the designated root of the filesystem graph.
type family IsRoot (v :: Vertex) :: Bool
type instance IsRoot Root = True
type instance IsRoot Drive = True
type instance IsRoot Remote = True
type instance IsRoot Home = True
type instance IsRoot Working = True
type instance IsRoot Directory = False
type instance IsRoot File = False

-- | If the start 'Vertex' is a root, the tree is rooted.
type Rooted a = IsRoot a ~ True

-- | A 'Path' is non-empty if it uses at least one 'Edge', which we know to be
-- true statically if the start and end vertices differ.  Currently, we only
-- test this for a 'Rooted' tree, since without overlapping type family
-- instances we'd have to write an instance for every valid path.
type NonEmpty a b = (Rooted a, IsRoot b ~ False)

-- | Test if a 'Vertex' is understood by the native platform.
type family IsNative (v :: Vertex) :: Bool
#ifndef __WINDOWS__
type instance IsNative Root = True
type instance IsNative Drive = False
#else
type instance IsNative Root = False
type instance IsNative Drive = True
#endif
type instance IsNative Remote = False
type instance IsNative Home = True
type instance IsNative Working = True
type instance IsNative Directory = True
type instance IsNative File = True

-- | A path is understood by the native platform if the start 'Vertex' is,
-- because every end 'Vertex' is.
type Native a = IsNative a ~ True

-- * Edge

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

deriving instance Eq (Edge a b)

instance Ord (Edge a b) where
    compare = compare `on` mono

deriving instance Show (Edge a b)

instance Mono (Edge a b) where
    type Monomorphic (Edge a b) = (Maybe Name, Vertex, Vertex)
    mono RootDirectory = (Nothing, Root, Directory)
    mono (DriveName n) = (Just n, Drive, Directory)
    mono (HostName n) = (Just n, Remote, Directory)
    mono HomeDirectory = (Nothing, Home, Directory)
    mono WorkingDirectory = (Nothing, Working, Directory)
    mono (DirectoryName n) = (Just n, Directory, Directory)
    mono (FileName n) = (Just n, Directory, File)
    mono (FileExtension n) = (Just n, File, File)

-- * Path

-- | Infix 'Path' type operator.
type (</>) = Path

-- | A path between vertices in the filesystem graph as a sequence of edges,
-- forming a free category under concatenation.
data Path :: Vertex -> Vertex -> * where
    Nil :: a </> a
    Cons :: a ->- b -> b </> c -> a </> c

instance Eq (Path a b) where
    (==) = (==) `on` mono

instance Ord (Path a b) where
    compare = compare `on` mono

deriving instance Show (Path a b)

instance (b ~ a) => Monoid (Path a b) where
    mempty = Nil
    mappend = (</>)

instance (a ~ Directory, b ~ a) => IsString (Path a b) where
    fromString = dir . fromString

instance Mono (Path a b) where
    type Monomorphic (Path a b) = [(Maybe Name, Vertex, Vertex)]
    mono Nil = []
    mono (Cons e p) = mono e : mono p

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

-- * Chunk

-- | Segments of a path representation.
data Chunk
    = ASCII !ByteString
      -- ^ Pass-through on encode; decode as ASCII.
    | Unicode !Text
      -- ^ Encode as appropriate; pass-through on decode.
    | Name !Name
      -- ^ Validate and encode/decode as appropriate.
  deriving (Eq, Ord, Show)

-- * Builder

-- | Difference list monoid for assembling chunks efficiently.
newtype Builder = Builder (Endo [Chunk]) deriving (Monoid)

instance Eq Builder where
    (==) = (==) `on` runBuilder

instance Ord Builder where
    compare = compare `on` runBuilder

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

-- | Construct a 'PathName' from a 'Builder' using the system locale.
locale :: Builder -> IO PathName
locale builder = do
    encoding <- getLocaleEncoding
#ifdef __POSIX__
    let c (ASCII b) = return b
        c (Unicode t) = Text.encodeLocale' encoding noNewlineTranslation t
        c (Name (ByteString b)) = return b
        c (Name (Text t)) = c (Unicode t)
    pathName . ByteString.concat <$> mapM c (runBuilder builder)
#else
    let c (ASCII b) = return (Text.decodeLatin1 b)
        c (Unicode t) = return t
        c (Name (ByteString b)) =
            Text.decodeLocale' encoding noNewlineTranslation b
        c (Name (Text t)) = return t
    pathName . Text.unpack . Text.concat <$> mapM c (runBuilder builder)
#endif

-- * Representation

-- | Syntax for representing a path in readable form.
data Representation = Posix | Windows deriving (Eq, Ord, Show)

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

-- * Resolve

-- | Resolve the edge from a native root vertex into a concrete 'PathName' for
-- the corresponding directory.
class (Rooted a, Native a) => Resolve a where
    resolve :: a ->- b -> IO PathName

#ifndef __WINDOWS__
instance Resolve Root where
#else
instance Resolve Drive where
#endif
    resolve = locale . render native . edge

instance Resolve Home where
    resolve _ = PathName.getHomeDirectory

instance Resolve Working where
    resolve _ = PathName.getCurrentDirectory

-- * Reify

-- | Reify paths into a concrete 'PathName'.
class Reify a where
    reify :: a -> IO PathName

instance Reify PathName where
    reify = return

instance (Resolve a, NonEmpty a b) => Reify (Path a b) where
    reify (Cons e es) = mappend <$> resolve e <*> locale (render native es)
    reify _ = fail "impossible"
