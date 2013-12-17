{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Path.Posix
    ( Component(..)
    , Separator(..)
    , Path(..)
    , (<.>)
    , (</>)
    , addExtension
    , addTrailingPathSeparator
    , combine
    , dropExtension
    , dropExtensions
    , dropFileName
    , dropTrailingPathSeparator
    , equalFilePath
    , extSeparator
    , hasExtension
    , hasTrailingPathSeparator
    , isAbsolute
    , isExtSeparator
    , isPathSeparator
    , isRelative
    , isSearchPathSeparator
    , isValid
    , joinPath
    , makeRelative
    , makeValid
    , normalise
    , pathSeparator
    , replaceBaseName
    , replaceDirectory
    , replaceExtension
    , replaceFileName
    , searchPathSeparator
    , splitDirectories
    , splitExtension
    , splitExtensions
    , splitFileName
    , splitPath
    , splitSearchPath
    , takeBaseName
    , takeDirectory
    , takeExtension
    , takeExtensions
    , takeFileName
    ) where

import Data.ByteString (ByteString)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Word (Word8)
import Prelude hiding (head, null)
import qualified Data.ByteString as ByteString

newtype Component = Component { unComponent :: ByteString } deriving (Eq, Ord, Monoid, IsString)

instance Show Component where
    showsPrec d x = showsPrec d (unComponent x)

newtype Separator = Separator { unSeparator :: Word8 } deriving (Eq, Ord)

instance Show Separator where
    showsPrec d x = showsPrec d (unSeparator x)

newtype Path = Path { unPath :: ByteString } deriving (Eq, Ord, IsString)

instance Show Path where
    showsPrec d x = showsPrec d (unPath x)

instance Monoid Path where
    mempty = Path mempty
    mappend = combine
    mconcat = joinPath

head :: Path -> Separator
head = Separator . ByteString.head . unPath

null :: Path -> Bool
null = ByteString.null . unPath

ord :: Char -> Separator
ord = Separator . toEnum . fromEnum

(<.>) :: Path -> Component -> Path
(<.>) = addExtension
infixr 7 <.>

(</>) :: Path -> Path -> Path
(</>) = combine
infixr 5 </>

addExtension :: Path -> Component -> Path
addExtension = error "System.Path.Posix.addExtension: not implemented"

addTrailingPathSeparator :: Path -> Path
addTrailingPathSeparator = error "System.Path.Posix.addTrailingPathSeparator: not implemented"

combine :: Path -> Path -> Path
combine = error "System.Path.Posix.combine: not implemented"

dropExtension :: Path -> Path
dropExtension = error "System.Path.Posix.dropExtension: not implemented"

dropExtensions :: Path -> Path
dropExtensions = error "System.Path.Posix.dropExtensions: not implemented"

dropFileName :: Path -> Path
dropFileName = error "System.Path.Posix.dropFileName: not implemented"

dropTrailingPathSeparator :: Path -> Path
dropTrailingPathSeparator = error "System.Path.Posix.dropTrailingPathSeparator: not implemented"

equalFilePath :: Path -> Path -> Bool
equalFilePath = error "System.Path.Posix.equalFilePath: not implemented"

extSeparator :: Separator
extSeparator = ord '.'

hasExtension :: Path -> Bool
hasExtension = error "System.Path.Posix.hasExtension: not implemented"

hasTrailingPathSeparator :: Path -> Bool
hasTrailingPathSeparator = error "System.Path.Posix.hasTrailingPathSeparator: not implemented"

isAbsolute :: Path -> Bool
isAbsolute p = not (null p) && isPathSeparator (head p)

isExtSeparator :: Separator -> Bool
isExtSeparator = (== extSeparator)

isPathSeparator :: Separator -> Bool
isPathSeparator = (== pathSeparator)

isRelative :: Path -> Bool
isRelative = not . isAbsolute

isSearchPathSeparator :: Separator -> Bool
isSearchPathSeparator = (== searchPathSeparator)

isValid :: Path -> Bool
isValid = error "System.Path.Posix.isValid: not implemented"

joinPath :: [Path] -> Path
joinPath = error "System.Path.Posix.joinPath: not implemented"

makeRelative :: Path -> Path -> Path
makeRelative = error "System.Path.Posix.makeRelative: not implemented"

makeValid :: Path -> Path
makeValid = error "System.Path.Posix.makeValid: not implemented"

normalise :: Path -> Path
normalise = error "System.Path.Posix.normalise: not implemented"

pathSeparator :: Separator
pathSeparator = ord '/'

replaceBaseName :: Path -> Component -> Path
replaceBaseName = error "System.Path.Posix.replaceBaseName: not implemented"

replaceDirectory :: Path -> Component -> Path
replaceDirectory = error "System.Path.Posix.replaceDirectory: not implemented"

replaceExtension :: Path -> Component -> Path
replaceExtension = error "System.Path.Posix.replaceExtension: not implemented"

replaceFileName :: Path -> Component -> Path
replaceFileName = error "System.Path.Posix.replaceFileName: not implemented"

searchPathSeparator :: Separator
searchPathSeparator = ord ':'

splitDirectories :: Path -> [Path]
splitDirectories = error "System.Path.Posix.splitDirectories: not implemented"

splitExtension :: Path -> (Component, Component)
splitExtension = error "System.Path.Posix.splitExtension: not implemented"

splitExtensions :: Path -> (Path, Component)
splitExtensions = error "System.Path.Posix.splitExtensions: not implemented"

splitFileName :: Path -> (Component, Component)
splitFileName = error "System.Path.Posix.splitFileName: not implemented"

splitPath :: Path -> [Path]
splitPath = error "System.Path.Posix.splitPath: not implemented"

splitSearchPath :: Component -> [Path]
splitSearchPath = error "System.Path.Posix.splitSearchPath: not implemented"

takeBaseName :: Path -> Component
takeBaseName = error "System.Path.Posix.takeBaseName: not implemented"

takeDirectory :: Path -> Path
takeDirectory = error "System.Path.Posix.takeDirectory: not implemented"

takeExtension :: Path -> Component
takeExtension = error "System.Path.Posix.takeExtension: not implemented"

takeExtensions :: Path -> Component
takeExtensions = error "System.Path.Posix.takeExtensions: not implemented"

takeFileName :: Path -> Path
takeFileName = error "System.Path.Posix.takeFileName: not implemented"
