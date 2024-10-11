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

import Text.Regex.PCRE 
import Data.Text.Zipper (getText)
import           Brick               (get, suspendAndResume)
import           Brick.Types         (EventM)
import           Brick.Widgets.Edit  (Editor, editor, editContentsL)
import           Brick.Widgets.List  (listElementsL)
import qualified Brick.Widgets.List  as L
import           Control.Monad.State (MonadIO (liftIO))
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector         as V
import           Dotf.Bundles
import           Dotf.Commands
import           Dotf.Types
import           Dotf.Utils
import           Lens.Micro          (Lens', lens, (^.))
import           Lens.Micro.Mtl      (use, (.=))
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
  deriving (Eq, Ord, Show)

data Tab
  = DotfileTab
  | BundleTab
  deriving (Eq)

instance Show Tab where
  show DotfileTab = "Dotfiles [1]"
  show BundleTab  = "Bundles [2]"

-- name, arch, osx, deb
data PkgRow = PkgRow String String String String

-- name, url, branch
data GitRow = GitRow String String String

data DialogChoice = Ok

data State = State
  { _focus          :: Focus
  , _tracked        :: L.List RName TrackedType
  , _untracked      :: L.List RName FilePath
  , _bundles        :: L.List RName Bundle
  , _packages       :: L.List RName PkgRow
  , _gitPackages    :: L.List RName GitRow
  , _scripts        :: L.List RName FilePath
  , _ignoreEdit     :: Editor String RName
  , _ignore         :: Bool
  , _newBundleEdit  :: Editor String RName
  , _newBundle      :: Bool
  , _filterEdit     :: Editor String RName
  , _filter         :: Bool
  , _error          :: Maybe GitError
  , _popup          :: Maybe (Popup DialogChoice RName)
  , _tab            :: Tab
  , _showAllTracked :: Bool
  }

-------------
-- Methods --
-------------

emptyState :: State
emptyState =
  State
    { _focus = FTracked
    , _tracked = L.list RTrackedList V.empty 1
    , _untracked = L.list RUntrackedList V.empty 1
    , _bundles = L.list RBundleList V.empty 1
    , _packages = L.list RPackageList V.empty 1
    , _gitPackages = L.list RGitPackageList V.empty 1
    , _scripts = L.list RScriptList V.empty 1
    , _ignoreEdit = editor RIgnoreEditor Nothing ""
    , _ignore = False
    , _newBundleEdit = editor RNewBundleEditor Nothing ""
    , _newBundle = False
    , _filterEdit = editor RFilterEditor Nothing ""
    , _filter = False
    , _error = Nothing
    , _popup = Nothing
    , _tab = DotfileTab
    , _showAllTracked = False
    }

withState :: [TrackedType] -> [FilePath] -> [Bundle] -> State
withState ts us bs =
  emptyState
    { _tracked = L.list RTrackedList (V.fromList ts) 1
    , _untracked = L.list RUntrackedList (V.fromList us) 1
    , _bundles = L.list RBundleList (V.fromList bs) 1
    , _packages = L.list RPackageList (V.fromList $ mkPkgRows bs) 1
    , _gitPackages = L.list RGitPackageList (V.fromList $ mkGitRows bs) 1
    , _scripts = L.list RScriptList (V.fromList $ collectScripts bs) 1
    }

mkPkgRows :: [Bundle] -> [PkgRow]
mkPkgRows bs = map mkRow $ collectNamedPackages bs
 where mkRow (NamedPackage n (Package a b c)) =
         PkgRow n (fromMaybe "" a) (fromMaybe "" b) (fromMaybe "" c)

mkGitRows :: [Bundle] -> [GitRow]
mkGitRows bs = map mkRow $ collectGitPackages bs
 where mkRow g = GitRow (gitName g) (gitUrl g) (fromMaybe "" $ gitBranch g)

hasFocus :: Focus -> State -> Bool
hasFocus expected st = expected == _focus st

countTracked :: State -> Int
countTracked s = V.length $ L.listElements $ _tracked s

countUntracked :: State -> Int
countUntracked s = V.length $ L.listElements $ _untracked s

bundleSel :: State -> Maybe Bundle
bundleSel state =
  let l = state ^. bundlesL
   in snd <$> L.listSelectedElement l

bundleSelFile :: State -> Maybe String
bundleSelFile state =
  let bundle = bundleSel state
      name = fmap bundleName bundle
   in fmap (++ ".yaml") name

trackedSel :: State -> Maybe TrackedType
trackedSel state =
  let l = state ^. trackedL
   in snd <$> L.listSelectedElement l

trackedSelFilePath :: State -> Maybe FilePath
trackedSelFilePath s = toPath <$> trackedSel s

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

errorL :: Lens' State (Maybe GitError)
errorL = lens _error (\s me -> s{_error = me})

tabL :: Lens' State Tab
tabL = lens _tab (\s t -> s{_tab = t})

showAllTrackedL :: Lens' State Bool
showAllTrackedL = lens _showAllTracked (\s t -> s{_showAllTracked = t})

-------------
-- Actions --
-------------

syncBundles :: DEvent ()
syncBundles = do
  bundles      <- liftIO loadBundles
  bundlesL     .= L.list RBundleList (V.fromList bundles) 1
  packagesL    .= L.list RPackageList (V.fromList $ mkPkgRows bundles) 1
  gitPackagesL .= L.list RGitPackageList (V.fromList $ mkGitRows bundles) 1
  scriptsL     .= L.list RScriptList (V.fromList $ collectScripts bundles) 1

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

syncDotfiles :: DEvent ()
syncDotfiles = syncTracked >> syncUntracked

syncTracked :: DEvent ()
syncTracked = do
  s        <- use showAllTrackedL
  filterEd <- use filterEditL
  files    <- liftIO $ unsafeListTracked s

  case head $ getText $ filterEd ^. editContentsL of
    "" -> trackedL .= L.list RTrackedList (mkTrackedList Nothing files) 1
    v  -> trackedL .= L.list RTrackedList (mkTrackedList (Just v) files) 1

syncUntracked :: DEvent ()
syncUntracked = do
  filterEd <- use filterEditL
  files    <- liftIO unsafeListUntracked

  case head $ getText $ filterEd ^. editContentsL of
    "" -> untrackedL .= L.list RUntrackedList (mkFileList Nothing files) 1
    v  -> untrackedL .= L.list RUntrackedList (mkFileList (Just v) files) 1

maybeEditFile :: Maybe FilePath -> DEvent ()
maybeEditFile Nothing = return ()
maybeEditFile (Just fp) = do
  state <- get
  suspendAndResume $ editFile fp >> pure state

maybeDiffFile :: Maybe FilePath -> DEvent ()
maybeDiffFile maybeFile = do
  state <- get
  suspendAndResume $ maybeDiff maybeFile >> pure state

mkFileList :: Maybe String -> [FilePath] -> V.Vector FilePath
mkFileList Nothing raw  = V.fromList raw
mkFileList (Just f) raw = V.fromList $ filter (=~ f) raw

mkTrackedList :: Maybe String -> [TrackedType] -> V.Vector TrackedType
mkTrackedList Nothing raw = V.fromList raw
mkTrackedList (Just f) raw = V.fromList $ filter (\v -> show v =~ f) raw
