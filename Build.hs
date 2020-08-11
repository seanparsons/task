{-# LANGUAGE ViewPatterns #-}

import Data.Char
import Data.Foldable
import Data.List
import Development.Shake
import Development.Shake.Cabal
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

buildDir :: FilePath
buildDir = "_build"

ghcVersionFile :: FilePath
ghcVersionFile = buildDir </> "ghc-version.txt"

taskApp :: String
taskApp = "task"

appSrc :: FilePath
appSrc = "app"

appHaskellSrc :: FilePath
appHaskellSrc = appSrc </> "src"

appSrcYamlFile :: FilePath
appSrcYamlFile = appSrc </> "package.yaml"

appBuildDir :: FilePath
appBuildDir = buildDir </> "app"

appBuildSrc :: FilePath
appBuildSrc = appBuildDir </> "src"

appBuildYamlFile :: FilePath
appBuildYamlFile = appBuildDir </> "package.yaml"

appBuildCabalFile :: FilePath
appBuildCabalFile = appBuildDir </> taskApp <.> ".cabal"

bundlePath :: FilePath
bundlePath = buildDir </> "bundle"

bundledApp :: FilePath
bundledApp = bundlePath </> taskApp <.> exe

needHaskellSrc :: Action ()
needHaskellSrc = do
  haskellSource <- getDirectoryFiles appHaskellSrc ["//*.hs"]
  let haskellFiles = fmap (appBuildSrc </>) haskellSource
  need haskellFiles

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "_build", shakeThreads = 0} $ do
  -- Root level needs.
  want ([bundledApp])
  -- Clean.
  phony "clean" $ do
    cmd_ (Cwd appSrc) "cabal v2-clean"
    removeFilesAfter "_build" ["//*"]
  -- Capture the version of GHC.
  ghcVersionFile %> \out -> do
    alwaysRerun
    Stdout stdout <- cmd "ghc --numeric-version"
    writeFileChanged out stdout
  appBuildYamlFile %> \out -> do
    need [appSrcYamlFile]
    copyFileChanged appSrcYamlFile appBuildYamlFile
  -- Get our cabal file by running hpack on the package.yaml file.
  appBuildCabalFile %> \out -> do
    need [appBuildYamlFile]
    needHaskellSrc
    cmd_ (Cwd appBuildDir) "hpack"
  -- Copy any .hs files into the builder folder.
  appBuildSrc <//> "*.hs" %> \out -> do
    let src =
          appHaskellSrc
            </> (dropDirectory1 $ dropDirectory1 $ dropDirectory1 out)
    need [src]
    copyFileChanged src out
  -- Build the server project with cabal.
  bundledApp %> \out -> do
    need [ghcVersionFile, appBuildCabalFile]
    needHaskellSrc
    cmd_ (Cwd appBuildDir) "cabal v2-build" ["-j", "exe:" <> taskApp]
    Stdout (trim -> exeLoc) <-
      cmd
        (Cwd appBuildDir)
        "cabal v2-exec -- which"
        [taskApp]
    copyFileChanged exeLoc out
