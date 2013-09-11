#!/usr/bin/env runhaskell

\begin{code}
{- OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Monad (liftM2, mplus)
import Data.List (isPrefixOf)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity

main = defaultMainWithHooks simpleUserHooks {
  hookedPrograms = [mysqlConfigProgram],

  confHook = \pkg flags -> do
    lbi <- confHook simpleUserHooks pkg flags
    bi  <- mysqlBuildInfo lbi
    return lbi {
      localPkgDescr = updatePackageDescription (Just bi, []) (localPkgDescr lbi)
    }
}

-- 'ConstOrId' is a Cabal compatibility hack.
-- see: https://github.com/scrive/hdbc-postgresql/commit/e9b2fbab07b8f55ae6a9e120ab0b98c433842a8b
class ConstOrId a b where
    constOrId :: a -> b

instance ConstOrId a a where
    constOrId = id

instance ConstOrId a (b -> a) where
    constOrId = const

mysqlConfigProgram = (simpleProgram "mysql_config") {
    programFindLocation = \verbosity -> constOrId $ liftM2 mplus
      (findProgramLocation verbosity "mysql_config")
      (findProgramLocation verbosity "mysql_config5")
  }

mysqlBuildInfo :: LocalBuildInfo -> IO BuildInfo
mysqlBuildInfo lbi = do
  let mysqlConfig = fmap words . rawSystemProgramStdoutConf normal
                    mysqlConfigProgram (withPrograms lbi)

  include <- mysqlConfig ["--include"]
  libs <- mysqlConfig ["--libs"]

  return emptyBuildInfo {
    extraLibDirs = map (drop 2) . filter ("-L" `isPrefixOf`) $ libs
  , extraLibs = map (drop 2) . filter ("-l" `isPrefixOf`) .
                filter (/= "-lmygcc") $ libs
  , includeDirs = map (drop 2) include
  }
\end{code}
