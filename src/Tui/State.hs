module Tui.State (
  DEvent,
  RName (..),
  Focus (..),
  Tab (..),
  PkgRow (..),
  GitRow (..),
  AppRow (..),
  DialogChoice (..),
  State (..),
  emptyState,
  withState,
  withState',
  hasFocus,
  countTracked,
  countUntracked,
  bundleSel,
  bundleSelFile,
  trackedSel,
  trackedSelFilePath,
  untrackedSel,
  focusL,
  trackedL,
  untrackedL,
  bundlesL,
  packagesL,
  gitPackagesL,
  scriptsL,
  appFavoritesL,
  appGamesL,
  appInternetL,
  appSettingsL,
  appSystemL,
  appOfficeL,
  ignoreEditL,
  ignoreL,
  newBundleEditL,
  newBundleL,
  filterEditL,
  filterL,
  commitEditL,
  commitL,
  errorL,
  tabL,
  showAllTrackedL,
  syncBundles,
  syncDotfiles,
  syncTracked,
  syncUntracked,
  syncApps,
  selectBundle,
  maybeEditFile,
  maybeDiffFile
) where

import           Brick                (get, modify, str, suspendAndResume)
import           Brick.Types          (EventM)
import qualified Brick.Widgets.Dialog as D
import           Brick.Widgets.Edit   (Editor, editContentsL, editor)
import           Brick.Widgets.List   (GenericList (listSelected),
                                       listElementsL, listMoveTo)
import qualified Brick.Widgets.List   as L
import           Control.Monad.State  (MonadIO (liftIO))
import           Data.Either          (fromRight)
import           Data.List            (unfoldr)
import           Data.Maybe           (fromMaybe)
import           Data.Text.Zipper     (getText)
import qualified Data.Vector          as V
import           Dotf.Bundles         (collectGitPackages, collectNamedPackages,
                                       collectScripts, loadBundles)
import qualified Dotf.Commands        as CMD
import           Dotf.Types           (AppCategory (..), AppConfig,
                                       AppLauncher (AppLauncher),
                                       Bundle (bundleName),
                                       Distro (Unsupported), ErrorOrAppConfig,
                                       ErrorOrBundles, ErrorOrFilePaths,
                                       ErrorOrTracked,
                                       GitPackage (gitBranch, gitName, gitUrl),
                                       NamedPackage (NamedPackage),
                                       Package (Package), Path (toPath),
                                       TrackedType)
import           Dotf.Utils           (editFile, runDiff)
import           Dotf.XMonad          (collectAppLaunchers, loadAppConfig)
import           Lens.Micro           (Lens', lens, (%~), (^.))
import           Lens.Micro.Mtl       (use, (.=))
import           Text.Regex.PCRE      ((=~))

-----------
-- Types --
-----------

type DEvent a = EventM RName State a

data RName
  = RTrackedList
  | RUntrackedList
  | RBundleList
  | RPackageList
  | RGitPackageList
  | RScriptList
  | RAppFavoritesList
  | RAppGamesList
  | RAppInternetList
  | RAppSettingsList
  | RAppSystemList
  | RAppOfficeList
  | RIgnoreEditor
  | RNewBundleEditor
  | RFilterEditor
  | RCommitEditor
  | RPopupOk
  deriving (Eq, Ord, Show)

data Focus
  = FTracked
  | FUntracked
  | FBundleList
  | FPackageList
  | FGitPackageList
  | FScriptList
  | FAppFavoritesList
  | FAppGamesList
  | FAppInternetList
  | FAppSettingsList
  | FAppSystemList
  | FAppOfficeList
  | FIgnoreEditor
  | FNewBundleEditor
  | FFilterEditor
  | FCommitEditor
  deriving (Eq, Ord, Show)

data Tab
  = DotfileTab
  | BundleTab
  | XMonadTab
  deriving (Eq)

instance Show Tab where
  show DotfileTab = "Dotfiles [1]"
  show BundleTab  = "Bundles [2]"
  show XMonadTab  = "XMonad [3]"

-- | Defines a table row for OS packages.
-- name, arch, osx, cask, deb
data PkgRow = PkgRow String String String String String

-- | Defines a table row for GIT packages.
-- name, url, branch
data GitRow = GitRow String String String

-- | Defines a table row for application launchers.
-- name, description
data AppRow = AppRow String String

data DialogChoice = Ok

data State = State {
  _focus          :: Focus,
  _tracked        :: L.List RName TrackedType,
  _untracked      :: L.List RName FilePath,
  _bundles        :: L.List RName Bundle,
  _packages       :: L.List RName PkgRow,
  _gitPackages    :: L.List RName GitRow,
  _scripts        :: L.List RName FilePath,
  _appFavorites   :: L.List RName AppRow,
  _appGames       :: L.List RName AppRow,
  _appInternet    :: L.List RName AppRow,
  _appSettings    :: L.List RName AppRow,
  _appSystem      :: L.List RName AppRow,
  _appOffice      :: L.List RName AppRow,
  _ignoreEdit     :: Editor String RName,
  _ignore         :: Bool,
  _newBundleEdit  :: Editor String RName,
  _newBundle      :: Bool,
  _filterEdit     :: Editor String RName,
  _filter         :: Bool,
  _commitEdit     :: Editor String RName,
  _commit         :: Bool,
  _error          :: Maybe [String],
  _popup          :: D.Dialog DialogChoice RName,
  _tab            :: Tab,
  _showAllTracked :: Bool
}

-------------
-- Methods --
-------------

-- | Create a new 'empty' state instance.
emptyState :: State
emptyState = State {
  _focus          = FTracked,
  _tracked        = L.list RTrackedList V.empty 1,
  _untracked      = L.list RUntrackedList V.empty 1,
  _bundles        = L.list RBundleList V.empty 1,
  _packages       = L.list RPackageList V.empty 1,
  _gitPackages    = L.list RGitPackageList V.empty 1,
  _scripts        = L.list RScriptList V.empty 1,
  _appFavorites   = L.list RAppFavoritesList V.empty 1,
  _appGames       = L.list RAppGamesList V.empty 1,
  _appInternet    = L.list RAppInternetList V.empty 1,
  _appSettings    = L.list RAppSettingsList V.empty 1,
  _appSystem      = L.list RAppSystemList V.empty 1,
  _appOffice      = L.list RAppOfficeList V.empty 1,
  _ignoreEdit     = editor RIgnoreEditor Nothing "",
  _ignore         = False,
  _newBundleEdit  = editor RNewBundleEditor Nothing "",
  _newBundle      = False,
  _filterEdit     = editor RFilterEditor Nothing "",
  _filter         = False,
  _commitEdit     = editor RCommitEditor Nothing "",
  _commit         = False,
  _error          = Nothing,
  _popup          = D.dialog (Just $ str "Error") (Just (RPopupOk, [("Ok", RPopupOk, Ok)])) 50,
  _tab            = DotfileTab,
  _showAllTracked = False
}

-- | Simple `State` constructor.
withState :: [TrackedType] -> [FilePath] -> [Bundle] -> AppConfig -> State
withState ts us bs cfg = emptyState {
  _tracked      = L.list RTrackedList (V.fromList ts) 1,
  _untracked    = L.list RUntrackedList (V.fromList us) 1,
  _bundles      = L.list RBundleList (V.fromList bs) 1,
  _packages     = L.list RPackageList (V.fromList $ mkPkgRows bs) 1,
  _gitPackages  = L.list RGitPackageList (V.fromList $ mkGitRows bs) 1,
  _scripts      = L.list RScriptList (V.fromList $ collectScripts bs) 1,
  _appFavorites = L.list RAppFavoritesList (V.fromList $ mkAppRows CFavorites cfg) 1,
  _appGames     = L.list RAppGamesList (V.fromList $ mkAppRows CGames cfg) 1,
  _appInternet  = L.list RAppInternetList (V.fromList $ mkAppRows CInternet cfg) 1,
  _appSettings  = L.list RAppSettingsList (V.fromList $ mkAppRows CSettings cfg) 1,
  _appSystem    = L.list RAppSystemList (V.fromList $ mkAppRows CSystem cfg) 1,
  _appOffice    = L.list RAppOfficeList (V.fromList $ mkAppRows COffice cfg) 1
}

-- | Error handling alternative to `withState`
withState' :: ErrorOrTracked -> ErrorOrFilePaths -> ErrorOrBundles -> ErrorOrAppConfig -> State
withState' (Left e) _ _ _ = emptyState { _error = Just (errMsg ["Unable to load tracked files!", ""] e) }
withState' _ (Left e) _ _ = emptyState { _error = Just (errMsg ["Unable to load untracked files!", ""] e) }
withState' _ _ (Left e) _ = emptyState { _error = Just (errMsg ["Unable to load bundles!", ""] e) }
withState' _ _ _ (Left e) = emptyState { _error = Just (errMsg ["Unable to load application config!", ""] e) }
withState' (Right a) (Right b) (Right c) (Right d) = withState a b c d

chunkString :: Int -> String -> [String]
chunkString n = unfoldr runSplit
  where runSplit []   = Nothing
        runSplit line = Just (splitAt n line)

breakLines :: Int -> String -> [String]
breakLines n = concatMap (chunkString n) . lines

errMsg :: Show a => [String] -> a -> [String]
errMsg msg err = msg ++ breakLines 45 (show err)

-- | Create TUI table rows for packages in provided bundles.
mkPkgRows :: [Bundle] -> [PkgRow]
mkPkgRows bs = map mkRow $ collectNamedPackages bs
 where mkRow (NamedPackage n (Package a b c d)) =
         PkgRow n (fromMaybe "" a) (fromMaybe "" b) (fromMaybe "" c) (fromMaybe "" d)

-- | Create TUI table rows for GIT packages in provided bundles.
-- FIXME show ignore list for GIT packages in TUI
mkGitRows :: [Bundle] -> [GitRow]
mkGitRows bs = map mkRow $ collectGitPackages Unsupported bs
 where mkRow g = GitRow (gitName g) (gitUrl g) (fromMaybe "" $ gitBranch g)

mkAppRows :: AppCategory -> AppConfig -> [AppRow]
mkAppRows cat cfg = map mkRow $ collectAppLaunchers cat cfg
 where mkRow (AppLauncher n d) = AppRow n d

-- | Tests if the given focus object is in fact in focus.
hasFocus :: Focus -> State -> Bool
hasFocus expected st = expected == _focus st

-- | Counts all tracked (visible) files.
countTracked :: State -> Int
countTracked s = V.length $ L.listElements $ _tracked s

-- | Counts all untracked (visible) files.
countUntracked :: State -> Int
countUntracked s = V.length $ L.listElements $ _untracked s

-- | Get the selected `Bundle` if any.
bundleSel :: State -> Maybe Bundle
bundleSel state =
  let l = state ^. bundlesL
   in snd <$> L.listSelectedElement l

-- | Get the selected bundle file if any.
bundleSelFile :: State -> Maybe String
bundleSelFile state =
  let bundle = bundleSel state
      name = fmap bundleName bundle
   in fmap (++ ".yaml") name

-- | Get the selected tracked file if any.
trackedSel :: State -> Maybe TrackedType
trackedSel state =
  let l = state ^. trackedL
   in snd <$> L.listSelectedElement l

-- | Get the selected tracked file path if any.
trackedSelFilePath :: State -> Maybe FilePath
trackedSelFilePath s = toPath <$> trackedSel s

-- | Get the selected untracked file path if any.
untrackedSel :: State -> Maybe FilePath
untrackedSel s =
  let l = s ^. untrackedL
   in case L.listSelectedElement l of
        Just (_, fp) -> Just fp
        _            -> Nothing

------------
-- Lenses --
------------

focusL :: Lens' State Focus
focusL = lens _focus (\s f -> s{_focus = f})

trackedL :: Lens' State (L.List RName TrackedType)
trackedL = lens _tracked (\s ts -> s{_tracked = ts})

untrackedL :: Lens' State (L.List RName FilePath)
untrackedL = lens _untracked (\s us -> s{_untracked = us})

bundlesL :: Lens' State (L.List RName Bundle)
bundlesL = lens _bundles (\s bs -> s{_bundles = bs})

packagesL :: Lens' State (L.List RName PkgRow)
packagesL = lens _packages (\s np -> s{_packages = np})

gitPackagesL :: Lens' State (L.List RName GitRow)
gitPackagesL = lens _gitPackages (\s gp -> s{_gitPackages = gp})

scriptsL :: Lens' State (L.List RName FilePath)
scriptsL = lens _scripts (\s ss -> s{_scripts = ss})

appFavoritesL :: Lens' State (L.List RName AppRow)
appFavoritesL = lens _appFavorites (\s af -> s{_appFavorites = af})

appGamesL :: Lens' State (L.List RName AppRow)
appGamesL = lens _appGames (\s ag -> s{_appGames = ag})

appInternetL :: Lens' State (L.List RName AppRow)
appInternetL = lens _appInternet (\s ai -> s{_appInternet = ai})

appSettingsL :: Lens' State (L.List RName AppRow)
appSettingsL = lens _appSettings (\s as -> s{_appSettings = as})

appSystemL :: Lens' State (L.List RName AppRow)
appSystemL = lens _appSystem (\s as -> s{_appSystem = as})

appOfficeL :: Lens' State (L.List RName AppRow)
appOfficeL = lens _appOffice (\s ao -> s{_appOffice = ao})

ignoreEditL :: Lens' State (Editor String RName)
ignoreEditL = lens _ignoreEdit (\s e -> s{_ignoreEdit = e})

ignoreL :: Lens' State Bool
ignoreL = lens _ignore (\s i -> s{_ignore = i})

newBundleEditL :: Lens' State (Editor String RName)
newBundleEditL = lens _newBundleEdit (\s e -> s{_newBundleEdit = e})

newBundleL :: Lens' State Bool
newBundleL = lens _newBundle (\s b -> s{_newBundle = b})

filterEditL :: Lens' State (Editor String RName)
filterEditL = lens _filterEdit (\s b -> s{_filterEdit = b})

filterL :: Lens' State Bool
filterL = lens _filter (\s b -> s{_filter = b})

commitEditL :: Lens' State (Editor String RName)
commitEditL = lens _commitEdit (\s e -> s { _commitEdit = e })

commitL :: Lens' State Bool
commitL = lens _commit (\s b -> s { _commit = b })

errorL :: Lens' State (Maybe [String])
errorL = lens _error (\s me -> s{_error = me})

tabL :: Lens' State Tab
tabL = lens _tab (\s t -> s{_tab = t})

showAllTrackedL :: Lens' State Bool
showAllTrackedL = lens _showAllTracked (\s t -> s {_showAllTracked = t})

-------------
-- Actions --
-------------

-- | Sync all application bundles.
-- This will reload all bundles from filesystem and recreate
-- the application state.
syncBundles :: DEvent ()
syncBundles = do
  bundles      <- liftIO loadBundles
  bundlesL     .= L.list RBundleList (V.fromList $ fromRight [] bundles) 1
  packagesL    .= L.list RPackageList (V.fromList $ mkPkgRows $ fromRight [] bundles) 1
  gitPackagesL .= L.list RGitPackageList (V.fromList $ mkGitRows $ fromRight [] bundles) 1
  scriptsL     .= L.list RScriptList (V.fromList $ collectScripts $ fromRight [] bundles) 1

-- | Select (or deselect) application bundle.
selectBundle :: Maybe Bundle -> DEvent ()
selectBundle (Just b) = do
  packagesL    .= L.list RPackageList (V.fromList $ mkPkgRows [b]) 1
  gitPackagesL .= L.list RGitPackageList (V.fromList $ mkGitRows [b]) 1
  scriptsL     .= L.list RScriptList (V.fromList $ collectScripts [b]) 1
selectBundle Nothing = do
  bundleList     <- use bundlesL
  let allBundles  = V.toList $ bundleList ^. listElementsL
  packagesL      .= L.list RPackageList (V.fromList $ mkPkgRows allBundles) 1
  gitPackagesL   .= L.list RGitPackageList (V.fromList $ mkGitRows allBundles) 1
  scriptsL       .= L.list RScriptList (V.fromList $ collectScripts allBundles) 1

-- | Sync all dot-files, both tracked and untracked.
syncDotfiles :: DEvent ()
syncDotfiles = syncTracked >> syncUntracked

-- | Sync all tracked dot-files.
syncTracked :: DEvent ()
syncTracked = do
  s        <- use showAllTrackedL
  lst      <- use trackedL
  filterEd <- use filterEditL
  files    <- liftIO $ CMD.unsafeListTracked s

  let idx = fromMaybe 0 $ listSelected lst

  case head $ getText $ filterEd ^. editContentsL of
    "" -> trackedL .= L.list RTrackedList (mkTrackedList Nothing files) 1
    v  -> trackedL .= L.list RTrackedList (mkTrackedList (Just v) files) 1

  modify (trackedL %~ listMoveTo idx)

-- | Sync all untracked dot-files.
syncUntracked :: DEvent ()
syncUntracked = do
  lst      <- use untrackedL
  filterEd <- use filterEditL
  files    <- liftIO CMD.unsafeListUntracked

  let idx = fromMaybe 0 $ listSelected lst

  case head $ getText $ filterEd ^. editContentsL of
    "" -> untrackedL .= L.list RUntrackedList (mkFileList Nothing files) 1
    v  -> untrackedL .= L.list RUntrackedList (mkFileList (Just v) files) 1

  modify (untrackedL %~ listMoveTo idx)

-- TODO rethink errors
syncApps :: DEvent ()
syncApps = do
  cfg <- liftIO loadAppConfig
  case cfg of
    Left e  -> do
      errorL .= Just (errMsg ["Unable to load application config!", ""] e)
    Right c -> do
      appFavoritesL .= L.list RAppFavoritesList (V.fromList $ mkAppRows CFavorites c) 1
      appGamesL     .= L.list RAppGamesList (V.fromList $ mkAppRows CGames c) 1
      appInternetL  .= L.list RAppInternetList (V.fromList $ mkAppRows CInternet c) 1
      appSettingsL  .= L.list RAppSettingsList (V.fromList $ mkAppRows CSettings c) 1
      appSystemL    .= L.list RAppSystemList (V.fromList $ mkAppRows CSystem c) 1
      appOfficeL    .= L.list RAppOfficeList (V.fromList $ mkAppRows COffice c) 1

-- | Edit a given filepath if any.
maybeEditFile :: Maybe FilePath -> DEvent ()
maybeEditFile Nothing = return ()
maybeEditFile (Just fp) = do
  state <- get
  suspendAndResume $ editFile fp >> pure state

-- | Show diff for a single file or entire index.
maybeDiffFile :: Maybe FilePath -> DEvent ()
maybeDiffFile maybeFile = do
  state <- get
  suspendAndResume $ runDiff maybeFile >> pure state

-- | Maybe filter the given list of filepaths.
mkFileList :: Maybe String -> [FilePath] -> V.Vector FilePath
mkFileList Nothing raw  = V.fromList raw
mkFileList (Just f) raw = V.fromList $ filter (=~ f) raw

-- | Maybe filter the given list of tracked files.
mkTrackedList :: Maybe String -> [TrackedType] -> V.Vector TrackedType
mkTrackedList Nothing raw  = V.fromList raw
mkTrackedList (Just f) raw = V.fromList $ filter (\v -> show v =~ f) raw

--setError :: [String] -> DEvent ()
--setError lines = do
  --state <- get

