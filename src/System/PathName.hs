{-# LANGUAGE Trustworthy #-}

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
--
module System.PathName
    ( -- * @PathName@
      PathName
    , FileName(..)
    , Reify(..)
    , build
    , decode
    , encode

      -- * @System.FilePath@
    , addExtension
    , addTrailingPathSeparator
    , combine
    -- , dropDrive
    , dropExtension
    , dropExtensions
    , dropFileName
    , dropTrailingPathSeparator
    -- , equalFilePath
    -- , extSeparator
    -- , getSearchPath
    -- , hasDrive
    , hasExtension
    , hasTrailingPathSeparator
    , isAbsolute
    -- , isDrive
    -- , isExtSeparator
    -- , isPathSeparator
    , isRelative
    -- , isSearchPathSeparator
    -- , isValid
    -- , joinDrive
    , joinPath
    , makeRelative
    -- , makeValid
    -- , normalise
    -- , pathSeparator
    -- , pathSeparators
    , replaceBaseName
    , replaceDirectory
    , replaceExtension
    , replaceFileName
    -- , searchPathSeparator
    , splitDirectories
    -- , splitDrive
    , splitExtension
    , splitExtensions
    , splitFileName
    , splitPath
    -- , splitSearchPath
    , takeBaseName
    , takeDirectory
    -- , takeDrive
    , takeExtension
    , takeExtensions
    , takeFileName

    {- Not sure these should be exported.  They're not legal lenses and have
     - really odd behavior in some cases.  It would be better to get lens to
     - fix its own lenses first and then mirror the legal versions here.
     -}

      -- -- * @System.FilePath.Lens@
    -- -- , (<.>=)
    -- -- , (<.>~)
    -- -- , (</>=)
    -- -- , (</>~)
    -- -- , (<<.>=)
    -- -- , (<<.>~)
    -- -- , (<</>=)
    -- -- , (<</>~)
    -- , basename
    -- , directory
    -- , extension
    -- , filename

      -- * @System.Directory@
    -- , Permissions(..)
    , canonicalizePath
    -- , copyFile
    -- , copyPermissions
    , createDirectory
    -- , createDirectoryIfMissing
    -- , doesDirectoryExist
    -- , doesFileExist
    -- , emptyPermissions
    -- , findExecutable
    -- , findFile
    -- , getAppUserDataDirectory
    , getCurrentDirectory
    -- , getDirectoryContents
    , getHomeDirectory
    , getModificationTime
    -- , getPermissions
    -- , getTemporaryDirectory
    -- , getUserDocumentsDirectory
    , makeRelativeToCurrentDirectory
    , removeDirectory
    -- , removeDirectoryRecursive
    , removeFile
    -- , renameDirectory
    -- , renameFile
    -- , setCurrentDirectory
    -- , setOwnerExecutable
    -- , setOwnerReadable
    -- , setOwnerSearchable
    -- , setOwnerWritable
    -- , setPermissions

      -- * @System.IO@
    -- , appendFile
    -- , openBinaryFile
    -- , openBinaryTempFile
    -- , openBinaryTempFileWithDefaultPermissions
    -- , openFile
    -- , openTempFile
    -- , openTempFileWithDefaultPermissions
    -- , readFile
    -- , withBinaryFile
    -- , withFile
    -- , writeFile
    ) where

import System.PathName.Internal
