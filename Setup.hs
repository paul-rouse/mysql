
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP                   #-}
{- OPTIONS_GHC -Wall #-}

#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 0 
#endif

import Control.Exception (IOException, catch)
import Control.Monad (liftM, msum, sequence)
import Data.List (isPrefixOf, isInfixOf, nub)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity

-- A Cabal 1.16 vs 1.18 compatibility hack, as in 1.18
-- findProgramLocation has a new (unused in this case) parameter.
-- ConstOrId adds this parameter when types say it is mandatory.
class ConstOrId a b where
    constOrId :: a -> b

instance ConstOrId a a where
    constOrId = id

instance ConstOrId a (b -> a) where
    constOrId = const


main = defaultMainWithHooks simpleUserHooks {
  hookedPrograms = [mysqlConfigProgram],

  confHook = \pkg flags -> do
    lbi <- confHook simpleUserHooks pkg flags
    bi  <- mysqlBuildInfo lbi
    return lbi {
      localPkgDescr = updatePackageDescription (Just bi, []) (localPkgDescr lbi)
    }
}

mysqlConfigProgram = (simpleProgram "mysql_config") {
    programFindLocation = \verbosity -> constOrId $ liftM msum $ sequence
#if MIN_VERSION_Cabal(1,24,0)
      [ (findProgramOnSearchPath verbosity [ProgramSearchPathDefault] "mysql_config")
      , (findProgramOnSearchPath verbosity [ProgramSearchPathDefault] "mysql_config5")
      , (findProgramOnSearchPath verbosity [ProgramSearchPathDefault] "mariadb_config")
      ]
#else
      [ (findProgramLocation verbosity "mysql_config")
      , (findProgramLocation verbosity "mysql_config5")
      , (findProgramLocation verbosity "mariadb_config")
      ]
#endif
  }

mysqlBuildInfo :: LocalBuildInfo -> IO BuildInfo
mysqlBuildInfo lbi = do
#if MIN_VERSION_Cabal(2,0,0)
  let mysqlConfig = fmap words . getDbProgramOutput normal
                    mysqlConfigProgram (withPrograms lbi)
#else
  let mysqlConfig = fmap words . rawSystemProgramStdoutConf normal
                    mysqlConfigProgram (withPrograms lbi)
#endif

  include <- mysqlConfig ["--include"]
  libs <- mysqlConfig ["--libs"]
  libsR <- mysqlConfig ["--libs_r"]
  libsSys' <- mysqlConfig ["--libs_sys"] `catch` libsFromError

  -- On some systems, `mysql_config` fails to give an error status even though
  -- it cannot handle `--libs_sys`.  The "Usage:" message lists libraries we do
  -- do not want to include, so recognise it for what it is!
  --
  let libsSys = if null $ filter ("Usage:" `isInfixOf`) libsSys' then libsSys'
                else []

  return emptyBuildInfo {
    extraLibDirs = map (drop 2) . filter ("-L" `isPrefixOf`) $
                   nub (libsSys ++ libs)
  , extraLibs = map (drop 2) . filter ("-l" `isPrefixOf`) .
                filter (/= "-lmygcc") $
                filter (/= "-lmysqlclient_r") $
                nub (libsSys ++ libs ++ libsR)
  , includeDirs = map (drop 2) include
  }
  where
    -- Recover from an error by returning an empty list.
    libsFromError :: IOException -> IO [String]
    libsFromError _ = return []
