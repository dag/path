{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Path.Windows
    ( Component(..)
    , Separator(..)
    , Path(..)
    , (<.>)
    , (</>)
    , addExtension
    , addTrailingPathSeparator
    , combine
    , dropDrive
    , dropExtension
    , dropExtensions
    , dropFileName
    , dropTrailingPathSeparator
    , equalFilePath
    , extSeparator
    , hasDrive
    , hasExtension
    , hasTrailingPathSeparator
    , isAbsolute
    , isDrive
    , isExtSeparator
    , isPathSeparator
    , isRelative
    , isSearchPathSeparator
    , isValid
    , joinDrive
    , joinPath
    , makeRelative
    , makeValid
    , normalise
    , pathSeparator
    , pathSeparators
    , replaceBaseName
    , replaceDirectory
    , replaceExtension
    , replaceFileName
    , searchPathSeparator
    , splitDirectories
    , splitDrive
    , splitExtension
    , splitExtensions
    , splitFileName
    , splitPath
    , splitSearchPath
    , takeBaseName
    , takeDirectory
    , takeDrive
    , takeExtension
    , takeExtensions
    , takeFileName
    ) where

import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Prelude hiding (head, null)
import qualified Data.Text as Text

newtype Component = Component { unComponent :: Text } deriving (Eq, Ord, Monoid, IsString)

instance Show Component where
    showsPrec d x = showsPrec d (unComponent x)

newtype Separator = Separator { unSeparator :: Char } deriving (Eq, Ord)

instance Show Separator where
    showsPrec d x = showsPrec d (unSeparator x)

newtype Path = Path { unPath :: Text } deriving (Eq, Ord, IsString)

instance Show Path where
    showsPrec d x = showsPrec d (unPath x)

instance Monoid Path where
    mempty = Path mempty
    mappend = combine
    mconcat = joinPath

head :: Path -> Separator
head = Separator . Text.head . unPath

null :: Path -> Bool
null = Text.null . unPath

(<.>) :: Path -> Component -> Path
(<.>) = addExtension
infixr 7 <.>

(</>) :: Path -> Path -> Path
(</>) = combine
infixr 5 </>

addExtension :: Path -> Component -> Path
addExtension = error "System.Path.Windows.addExtension: not implemented"

addTrailingPathSeparator :: Path -> Path
addTrailingPathSeparator = error "System.Path.Windows.addTrailingPathSeparator: not implemented"

combine :: Path -> Path -> Path
combine = error "System.Path.Windows.combine: not implemented"

dropDrive :: Path -> Path
dropDrive = error "System.Path.Windows.dropDrive: not implemented"

dropExtension :: Path -> Path
dropExtension = error "System.Path.Windows.dropExtension: not implemented"

dropExtensions :: Path -> Path
dropExtensions = error "System.Path.Windows.dropExtensions: not implemented"

dropFileName :: Path -> Path
dropFileName = error "System.Path.Windows.dropFileName: not implemented"

dropTrailingPathSeparator :: Path -> Path
dropTrailingPathSeparator = error "System.Path.Windows.dropTrailingPathSeparator: not implemented"

equalFilePath :: Path -> Path -> Bool
equalFilePath = error "System.Path.Windows.equalFilePath: not implemented"

extSeparator :: Separator
extSeparator = Separator '.'

hasDrive :: Path -> Bool
hasDrive = error "System.Path.Windows.hasDrive: not implemented"

hasExtension :: Path -> Bool
hasExtension = error "System.Path.Windows.hasExtension: not implemented"

hasTrailingPathSeparator :: Path -> Bool
hasTrailingPathSeparator = error "System.Path.Windows.hasTrailingPathSeparator: not implemented"

isAbsolute :: Path -> Bool
isAbsolute = error "System.Path.Windows.isAbsolute: not implemented"

isDrive :: Path -> Bool
isDrive = error "System.Path.Windows.isDrive: not implemented"

isExtSeparator :: Separator -> Bool
isExtSeparator = (== extSeparator)

isPathSeparator :: Separator -> Bool
isPathSeparator = (`elem` pathSeparators)

isRelative :: Path -> Bool
isRelative = not . isAbsolute

isSearchPathSeparator :: Separator -> Bool
isSearchPathSeparator = (== searchPathSeparator)

isValid :: Path -> Bool
isValid = error "System.Path.Windows.isValid: not implemented"

joinDrive :: Path -> Path -> Path
joinDrive = error "System.Path.Windows.joinDrive: not implemented"

joinPath :: [Path] -> Path
joinPath = error "System.Path.Windows.joinPath: not implemented"

makeRelative :: Path -> Path -> Path
makeRelative = error "System.Path.Windows.makeRelative: not implemented"

makeValid :: Path -> Path
makeValid = error "System.Path.Windows.makeValid: not implemented"

normalise :: Path -> Path
normalise = error "System.Path.Windows.normalise: not implemented"

pathSeparator :: Separator
pathSeparator = Separator '\\'

pathSeparators :: [Separator]
pathSeparators = [Separator '\\', Separator '/']

replaceBaseName :: Path -> Component -> Path
replaceBaseName = error "System.Path.Windows.replaceBaseName: not implemented"

replaceDirectory :: Path -> Component -> Path
replaceDirectory = error "System.Path.Windows.replaceDirectory: not implemented"

replaceExtension :: Path -> Component -> Path
replaceExtension = error "System.Path.Windows.replaceExtension: not implemented"

replaceFileName :: Path -> Component -> Path
replaceFileName = error "System.Path.Windows.replaceFileName: not implemented"

searchPathSeparator :: Separator
searchPathSeparator = Separator ';'

splitDirectories :: Path -> [Path]
splitDirectories = error "System.Path.Windows.splitDirectories: not implemented"

splitDrive :: Path -> (Path, Path)
splitDrive = error "System.Path.Windows.splitDrive: not implemented"

splitExtension :: Path -> (Component, Component)
splitExtension = error "System.Path.Windows.splitExtension: not implemented"

splitExtensions :: Path -> (Path, Component)
splitExtensions = error "System.Path.Windows.splitExtensions: not implemented"

splitFileName :: Path -> (Component, Component)
splitFileName = error "System.Path.Windows.splitFileName: not implemented"

splitPath :: Path -> [Path]
splitPath = error "System.Path.Windows.splitPath: not implemented"

splitSearchPath :: Component -> [Path]
splitSearchPath = error "System.Path.Windows.splitSearchPath: not implemented"

takeBaseName :: Path -> Component
takeBaseName = error "System.Path.Windows.takeBaseName: not implemented"

takeDirectory :: Path -> Path
takeDirectory = error "System.Path.Windows.takeDirectory: not implemented"

takeDrive :: Path -> Path
takeDrive = error "System.Path.Windows.takeDrive: not implemented"

takeExtension :: Path -> Component
takeExtension = error "System.Path.Windows.takeExtension: not implemented"

takeExtensions :: Path -> Component
takeExtensions = error "System.Path.Windows.takeExtensions: not implemented"

takeFileName :: Path -> Path
takeFileName = error "System.Path.Windows.takeFileName: not implemented"
