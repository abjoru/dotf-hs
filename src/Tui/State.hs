module Tui.State (
  DEvent,
  RName (..),
  Focus (..),
  Tab (..),
  PkgRow (..),
  GitRow (..),
  DialogChoice (..),
  State (..),
  emptyState,
  withState,
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
  selectBundle,
  maybeEditFile,
  maybeDiffFile
) where

import           Brick               (get, modify, suspendAndResume)
import           Brick.Types         (EventM)
import           Brick.Widgets.Edit  (Editor, editContentsL, editor)
import           Brick.Widgets.List  (GenericList (listSelected), listElementsL,
                                      listMoveTo)
import qualified Brick.Widgets.List  as L
import           Control.Monad.State (MonadIO (liftIO))
import           Data.Maybe          (fromMaybe)
import           Data.Text.Zipper    (getText)
import qualified Data.Vector         as V
import           Dotf.Bundles
import qualified Dotf.Commands       as CMD
import           Dotf.Types
import           Dotf.Utils
import           Lens.Micro          (Lens', lens, (%~), (^.))
import           Lens.Micro.Mtl      (use, (.=))
import           Text.Regex.PCRE
import           Tui.Popup           (Popup)

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
  | RIgnoreEditor
  | RNewBundleEditor
  | RFilterEditor
  | RCommitEditor
  deriving (Eq, Ord, Show)

data Focus
  = FTracked
  | FUntracked
  | FBundleList
  | FPackageList
  | FGitPackageList
  | FScriptList
  | FIgnoreEditor
  | FNewBundleEditor
  | FFilterEditor
  | FCommitEditor
  deriving (Eq, Ord, Show)

data Tab
  = DotfileTab
  | BundleTab
  deriving (Eq)

instance Show Tab where
  show DotfileTab = "Dotfiles [1]"
  show BundleTab  = "Bundles [2]"

-- | Defines a table row for OS packages.
-- name, arch, osx, deb
data PkgRow = PkgRow String String String String

-- | Defines a table row for GIT packages.
-- name, url, branch
data GitRow = GitRow String String String

data DialogChoice = Ok

data State = State {
  _focus          :: Focus,
  _tracked        :: L.List RName TrackedType,
  _untracked      :: L.List RName FilePath,
  _bundles        :: L.List RName Bundle,
  _packages       :: L.List RName PkgRow,
  _gitPackages    :: L.List RName GitRow,
  _scripts        :: L.List RName FilePath,
  _ignoreEdit     :: Editor String RName,
  _ignore         :: Bool,
  _newBundleEdit  :: Editor String RName,
  _newBundle      :: Bool,
  _filterEdit     :: Editor String RName,
  _filter         :: Bool,
  _commitEdit     :: Editor String RName,
  _commit         :: Bool,
  _error          :: Maybe GitError,
  _popup          :: Maybe (Popup DialogChoice RName),
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
  _ignoreEdit     = editor RIgnoreEditor Nothing "",
  _ignore         = False,
  _newBundleEdit  = editor RNewBundleEditor Nothing "",
  _newBundle      = False,
  _filterEdit     = editor RFilterEditor Nothing "",
  _filter         = False,
  _commitEdit     = editor RCommitEditor Nothing "",
  _commit         = False,
  _error          = Nothing,
  _popup          = Nothing,
  _tab            = DotfileTab,
  _showAllTracked = False
}

-- | Simple `State` constructor.
withState :: [TrackedType] -> [FilePath] -> [Bundle] -> State
withState ts us bs = emptyState {
  _tracked = L.list RTrackedList (V.fromList ts) 1,
  _untracked = L.list RUntrackedList (V.fromList us) 1,
  _bundles = L.list RBundleList (V.fromList bs) 1,
  _packages = L.list RPackageList (V.fromList $ mkPkgRows bs) 1,
  _gitPackages = L.list RGitPackageList (V.fromList $ mkGitRows bs) 1,
  _scripts = L.list RScriptList (V.fromList $ collectScripts bs) 1
}

-- | Create TUI table rows for packages in provided bundles.
mkPkgRows :: [Bundle] -> [PkgRow]
mkPkgRows bs = map mkRow $ collectNamedPackages bs
 where mkRow (NamedPackage n (Package a b c)) =
         PkgRow n (fromMaybe "" a) (fromMaybe "" b) (fromMaybe "" c)

-- | Create TUI table rows for GIT packages in provided bundles.
-- FIXME show ignore list for GIT packages in TUI
mkGitRows :: [Bundle] -> [GitRow]
mkGitRows bs = map mkRow $ collectGitPackages Unsupported bs
 where mkRow g = GitRow (gitName g) (gitUrl g) (fromMaybe "" $ gitBranch g)

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

errorL :: Lens' State (Maybe GitError)
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
  bundlesL     .= L.list RBundleList (V.fromList bundles) 1
  packagesL    .= L.list RPackageList (V.fromList $ mkPkgRows bundles) 1
  gitPackagesL .= L.list RGitPackageList (V.fromList $ mkGitRows bundles) 1
  scriptsL     .= L.list RScriptList (V.fromList $ collectScripts bundles) 1

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
