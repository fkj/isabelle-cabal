module Isabelle.Cabal
  ( isabelleCabalMain
  , isabelleCabalMainHooks
  , isabelleCabalHooks) where

import Distribution.Simple
    ( UserHooks(buildHook, hookedPrograms),
      defaultMainWithHooks,
      simpleUserHooks )
import Distribution.Simple.Setup as DS
    ( BuildFlags(buildVerbosity), fromFlagOrDefault )
import Distribution.Types.Lens
    ( HasBuildInfo(hsSourceDirs, buildInfo, customFieldsBI),
      PackageDescription,
      BuildInfo,
      libName,
      allLibraries )
import Distribution.Types.UnqualComponentName
    ( UnqualComponentName, unUnqualComponentName )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo(withPrograms, buildDir) )
import Distribution.Verbosity ( normal )
import Distribution.Simple.Utils (die', info, intercalate, withTempDirectory)
import Distribution.Simple.Program
    ( Program(programFindVersion),
      ProgArg,
      findProgramVersion,
      getDbProgramOutput,
      simpleProgram )
import Distribution.Types.LibraryName ( libraryNameString )
import Distribution.Simple.Program.Db ()
import Distribution.Utils.Path ( unsafeMakeSymbolicPath )
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Char (isDigit)
import Control.Lens
    ( Traversal',
      (&),
      view,
      (<.),
      reindexed,
      selfIndex,
      (%~),
      itraverseOf,
      IndexedTraversal' )
import Control.Monad (foldM)

isabelleCabalMain :: IO ()
isabelleCabalMain = defaultMainWithHooks isabelleCabalHooks

isabelleCabalMainHooks :: IO ()
isabelleCabalMainHooks = defaultMainWithHooks $ isabelleCabalHooks

isabelleCabalHooks :: UserHooks
isabelleCabalHooks = simpleUserHooks { buildHook = isabelleCabalBuildHook
                                       , hookedPrograms = isabelleProgram : hookedPrograms simpleUserHooks }

isabelleCabalBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
isabelleCabalBuildHook pkg lbi hooks flags = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity flags)
  withTempDirectory verbosity (buildDir lbi) "isabelle-sources" $ \isabelleSources -> do
    pkg' <- runIsabelle pkg lbi flags isabelleSources
    buildHook simpleUserHooks pkg' lbi hooks flags

data IsabelleBuildable where
  IsabelleBuildable :: (HasBuildInfo a) => Traversal' PackageDescription a -> (a -> Maybe UnqualComponentName) -> IsabelleBuildable

isabelleBuildables :: [IsabelleBuildable]
isabelleBuildables =
  [ IsabelleBuildable allLibraries (libraryNameString . view libName) ]

itagged :: Traversal' s a -> (a -> b) -> IndexedTraversal' b s a
itagged l f = reindexed f (l . selfIndex)

runIsabelle :: PackageDescription -> LocalBuildInfo -> BuildFlags -> FilePath -> IO PackageDescription
runIsabelle pkg lbi flags isabelleSources = do
  foldM (&) pkg $
    [ itraverseOf focus $ buildIsabelleLib lbi flags isabelleSources
    | IsabelleBuildable component getName <- isabelleBuildables
    , let focus = itagged component getName <. buildInfo ]

buildIsabelleLib :: LocalBuildInfo -> BuildFlags -> FilePath -> Maybe UnqualComponentName -> BuildInfo -> IO BuildInfo
buildIsabelleLib lbi flags isabelleSources name bi = case session of
  Nothing -> pure bi
  Just sessionName -> buildIsabelleLib' lbi flags isabelleSources name bi (splitOn " " sessionName)
  where
    session = lookup "x-isabelle-session" $ view customFieldsBI bi

buildIsabelleLib' :: LocalBuildInfo -> BuildFlags -> FilePath -> Maybe UnqualComponentName -> BuildInfo -> [String] -> IO BuildInfo
buildIsabelleLib' lbi flags isabelleSources name bi sessionInput = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity flags)
  let dirName = case name of
                  Nothing -> "library"
                  Just x -> unUnqualComponentName x
  putStrLn $ "Buiding Isabelle theories for " <> dirName <> "..."
  case sessionInput of
    [] -> die' verbosity "Isabelle session name cannot be empty"
    [a] -> info verbosity $ "Isabelle session: " <> a
    _ -> die' verbosity "Only one Isabelle session name can be specified"
  case patternInput of
    Nothing -> die' verbosity "No Isabelle export file pattern specified"
    Just a -> info verbosity $ "Isabelle export file patterns: " <> intercalate ", " a
  case sourcesInput of
    Nothing -> pure ()
    Just a -> info verbosity $ "Isabelle source directories: " <> intercalate ", " a
  case optionsInput of
    Nothing -> pure ()
    Just a -> info verbosity $ "Isabelle options: " <> intercalate ", " a
  case pruningInput of
    Nothing -> pure ()
    Just [] -> pure ()
    Just [p] -> if all isDigit p
                then info verbosity $ "Isabelle pruning level: " <> p
                else die' verbosity "Pruning level must be a positive integer"
    Just _ ->  die' verbosity "Only one pruning level can be specified"

  let arguments = (concat $ catMaybes [addOptionDashes sourcesInput "-d"
                                      , addOptionDashes optionsInput "-o"
                                      , addOptionDashes pruningInput "-p"
                                      , addOptionDashes patternInput "-x"
                                      , Just ["-O" <> isabelleSources <> "/" <> dirName]])
                  ++ sessionInput
  isabelleOutput <- getDbProgramOutput verbosity isabelleProgram (withPrograms lbi) (["export"] ++ arguments)
  putStrLn $ formatIsabelleOutput isabelleOutput

  return $ bi
    & hsSourceDirs %~ (unsafeMakeSymbolicPath (isabelleSources <> "/" <> dirName):)

  where
    sourcesInput = splitOn " " <$> (lookup "x-isabelle-src-dir" $ view customFieldsBI bi)
    optionsInput = splitOn " " <$> (lookup "x-isabelle-options" $ view customFieldsBI bi)
    pruningInput = splitOn " " <$> (lookup "x-isabelle-prune" $ view customFieldsBI bi)
    patternInput = splitOn " " <$> (lookup "x-isabelle-pattern" $ view customFieldsBI bi)

addOptionDashes :: Maybe [ProgArg] -> ProgArg -> Maybe [ProgArg]
addOptionDashes Nothing _ = Nothing
addOptionDashes (Just xs) option = Just (map (\a -> option <> a) xs)

formatIsabelleOutput :: String -> String
formatIsabelleOutput output =
  let outLines = splitOn "\n" output in
    let modules = map process (filter (\x -> x /= []) outLines) in
      "Generated modules:\n" <> intercalate "\n" (map (\x -> " - " <> x) modules)
  where
    process line =
      case splitOn " " line of
        [] -> ""
        "export" : ws -> head $ splitOn ".hs" $ last $ splitOn "/" $ last ws
        _ -> ""

isabelleProgram :: Program
isabelleProgram = (simpleProgram "isabelle") {
  programFindVersion = findProgramVersion "version" id
  }
