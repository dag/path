{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified System.PathName as PathName
import System.PathName.Internal (PathName(..))

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

-- | Make an 'Either' out of a 'Name'.
name :: Name -> Either ByteString Text
name (ByteString bs) = Left bs
name (Text ts) = Right ts

-- * File

-- | In UNIX tradition, \"everything is a file\" but there are different types
-- of files.  This 'File' type is promoted to a kind and defines the set of
-- vertices in the filesystem graph and conversely the class of objects in the
-- 'Path' category.  You can think of 'File' as representing a particular inode
-- on a POSIX system.
data File
    = Root  -- ^ The root directory of the current process.
    | Drive  -- ^ The root directory of particular drive.
    | Remote  -- ^ The root directory of a remote machine.
    | Home  -- ^ The home directory of the current user.
    | Working  -- ^ The current working directory.
    | Directory  -- ^ An ordinary named directory.
    | Regular  -- ^ A named file that is not a directory.
  deriving (Eq, Ord, Show)

-- | Test if a 'File' is the designated root of the filesystem graph.
type family IsRoot (v :: File) :: Bool
type instance IsRoot Root = True
type instance IsRoot Drive = True
type instance IsRoot Remote = True
type instance IsRoot Home = True
type instance IsRoot Working = True
type instance IsRoot Directory = False
type instance IsRoot Regular = False

-- | If the start 'File' is a root, the tree is rooted.
type Rooted a = IsRoot a ~ True

-- | A 'Path' is non-empty if it uses at least one 'Link', which we know to be
-- true statically if the start and end vertices differ.  Currently, we only
-- test this for a 'Rooted' tree, since without overlapping type family
-- instances we'd have to write an instance for every valid path.
type NonEmpty a b = (Rooted a, IsRoot b ~ False)

-- | Test if a 'File' is understood by the native platform.
type family IsNative (v :: File) :: Bool
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
type instance IsNative Regular = True

-- | A path is understood by the native platform if the start 'File' is,
-- because every end 'File' is.
type Native a = IsNative a ~ True

-- * Link

-- | Infix 'Link' type operator.
type (->-) = Link

-- | Links between files on the filesystem.  This type defines the set of edges
-- in the filesystem graph, but actually /not/ the class of morphisms of the
-- 'Path' category; rather, 'Path' itself defines the morphisms.  You can think
-- of 'Link' as representing a hard link on a POSIX system, and remember that
-- hard links are how files are named and directories become hierarchical.
data Link :: File -> File -> * where
    RootDirectory :: Root ->- Directory
    DriveName :: !Name -> Drive ->- Directory
    HostName :: !Name -> Remote ->- Directory
    HomeDirectory :: Home ->- Directory
    WorkingDirectory :: Working ->- Directory
    DirectoryName :: !Name -> Directory ->- Directory
    FileName :: !Name -> Directory ->- Regular
    FileExtension :: !Name -> Regular ->- Regular

deriving instance Eq (Link a b)

instance Ord (Link a b) where
    compare = compare `on` mono

deriving instance Show (Link a b)

instance Mono (Link a b) where
    type Monomorphic (Link a b) = (Maybe Name, File, File)
    mono RootDirectory = (Nothing, Root, Directory)
    mono (DriveName n) = (Just n, Drive, Directory)
    mono (HostName n) = (Just n, Remote, Directory)
    mono HomeDirectory = (Nothing, Home, Directory)
    mono WorkingDirectory = (Nothing, Working, Directory)
    mono (DirectoryName n) = (Just n, Directory, Directory)
    mono (FileName n) = (Just n, Directory, Regular)
    mono (FileExtension n) = (Just n, Regular, Regular)

-- * Path

-- | Infix 'Path' type operator.
type (</>) = Path

-- | A path from one 'File' to another.  This is a sequence of links that we
-- need to follow to reach the target file @b@ starting from the source file
-- @a@.  It is the free category generated by the filesystem graph, where the
-- empty path is the identity morphism and concatenation is the composition of
-- morphisms.
data Path :: File -> File -> * where
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
    type Monomorphic (Path a b) = [(Maybe Name, File, File)]
    mono Nil = []
    mono (Cons e p) = mono e : mono p

instance PathName.Reify (Path a b) where
    reify Nil = return mempty
    reify es@(Cons e _) = mappend
        <$> rooted e
        <*> uncurry PathName.build (components es)
      where
        rooted RootDirectory = return (PathName "/")
        rooted (DriveName n) = do
            PathName p <- PathName.build [name n] []
            return $ PathName (mappend p ":")
        rooted HomeDirectory = PathName.getHomeDirectory
        rooted WorkingDirectory = PathName.getCurrentDirectory
        rooted _ = return mempty

-- | Extract directory and filename components from a 'Path'.
components :: a </> b -> ([Either ByteString Text], [Either ByteString Text])
components Nil = ([], [])
components (Cons e es) = case e of
    DirectoryName n -> (name n : ps, fs)
    FileName n -> (ps, name n : fs)
    FileExtension n -> (ps, name n : fs)
    _ -> (ps, fs)
  where
    (ps, fs) = components es

-- | Concatenate two paths.
(</>) :: a </> b -> b </> c -> a </> c
Nil </> b = b
Cons e a </> b = Cons e (a </> b)

-- | Prepend a 'drive' name to a path.
(<:>) :: Name -> Directory </> b -> Drive </> b
n <:> b = drive n </> b

-- | Append a file extension to a file path.
(<.>) :: a </> Regular -> Name -> a </> Regular
a <.> n = a </> ext n

-- | Construct a 'Path' from a 'Link'.
link :: a ->- b -> a </> b
link = flip Cons Nil

-- | The root directory.
root :: Root </> Directory
root = link RootDirectory

-- | A drive letter / device name.
drive :: Name -> Drive </> Directory
drive = link . DriveName

-- | A remote host.
host :: Name -> Remote </> Directory
host = link . HostName

-- | The home directory of the current user.
home :: Home </> Directory
home = link HomeDirectory

-- | The current working directory.
cwd :: Working </> Directory
cwd = link WorkingDirectory

-- | A directory.
dir :: Name -> Directory </> Directory
dir = link . DirectoryName

-- | A file name.
file :: Name -> Directory </> Regular
file = link . FileName

-- | A file extension.
ext :: Name -> Regular </> Regular
ext = link . FileExtension

-- * Reference

-- | How a 'ResolvedPath' was resolved.
data Reference = Relative | Absolute | Canonical deriving (Eq, Ord, Show)

-- * ResolvedPath

-- | A resolved path with a 'Reference'.
data ResolvedPath :: Reference -> * where
    RelativePath :: !PathName -> ResolvedPath Relative
    AbsolutePath :: !PathName -> ResolvedPath Absolute
    CanonicalPath :: !PathName -> ResolvedPath Canonical

deriving instance Eq (ResolvedPath ref)

instance Ord (ResolvedPath ref) where
    compare = compare `on` mono

instance Show (ResolvedPath ref) where
    showsPrec d x = showsPrec d (snd (mono x))

instance Mono (ResolvedPath ref) where
    type Monomorphic (ResolvedPath ref) = (Reference, PathName)
    mono (RelativePath pn) = (Relative, pn)
    mono (AbsolutePath pn) = (Absolute, pn)
    mono (CanonicalPath pn) = (Canonical, pn)

instance PathName.Reify (ResolvedPath ref) where
    reify = return . snd . mono

-- * Resolve

-- | Resolve a path into a 'ResolvedPath', remembering the 'Reference'.
class Resolve path where
    relative :: path -> IO (ResolvedPath Relative)
    absolute :: path -> IO (ResolvedPath Absolute)
    canonical :: path -> IO (ResolvedPath Canonical)

instance Resolve (ResolvedPath ref) where
    relative rp = case rp of
        RelativePath _ -> return rp
        _ -> do
            pn <- PathName.makeRelativeToCurrentDirectory
                <=< PathName.reify $ rp
            if PathName.isRelative pn then
                return (RelativePath pn)
            else
                {- TODO proper Exception or traverse upwards with "../" -}
                fail "System.Path.resolve: cwd not parent of path"
    absolute rp = case rp of
        RelativePath pn -> AbsolutePath . (<> pn)
            <$> PathName.getCurrentDirectory
        AbsolutePath _ -> return rp
        CanonicalPath pn -> return (AbsolutePath pn)
    canonical rp = case rp of
        RelativePath _ -> canonical <=< absolute $ rp
        AbsolutePath pn -> CanonicalPath <$> PathName.canonicalizePath pn
        CanonicalPath _ -> return rp

instance (Rooted a, Native a, NonEmpty a b) => Resolve (Path a b) where
    relative = relative <=< absolute
    absolute = fmap AbsolutePath . PathName.reify
    canonical = fmap CanonicalPath . PathName.canonicalizePath
        <=< PathName.reify
