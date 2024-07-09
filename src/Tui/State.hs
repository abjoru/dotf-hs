module Tui.State (
  RName,
  Focus,
  DialogChoice(..),
  Tab(..),
  State(..),

  emptyState,

  focusL,
  trackedL,
  trackedSelL,
  untrackedL,
  untrackedSelL,
  bundlesL,
  bundleSelL,
  ignoreEditL,
  errorL
) where

import           Brick.Widgets.Edit (Editor, editor)
import           Data.Dotf          (Bundle, GitError, TrackedType)
import           Lens.Micro         (Lens', lens)
import           Tui.Popup          (Popup)

-- Not sure about this one?
-- Do we track specific resources?
-- Would a tab be a viewport in this sense?
data RName
  = Dotfiles
  | Bundles
  deriving (Eq, Ord, Show)

data Focus
  = Tracked
  | Untracked
  | IgnoreDialog
  | HelpDialog
  | ErrorDialog
  | BundleList
  | PackageTable
  | GitPackageTable
  | ScriptList
  deriving (Eq, Ord, Show)

data DialogChoice = Ok

data Tab
  = DotfileTab
  | BundleTab
  deriving Eq

instance Show Tab where
  show DotfileTab = "Dotfiles [1]"
  show BundleTab  = "Bundles [2]"

data State = State
  { _focus        :: Focus
  , _tracked      :: [TrackedType]
  , _trackedSel   :: Maybe TrackedType
  , _untracked    :: [FilePath]
  , _untrackedSel :: Maybe FilePath
  , _bundles      :: [Bundle]
  , _bundleSel    :: Maybe Bundle
  , _ignoreEdit   :: Editor String RName
  , _error        :: Maybe GitError
  , _popup        :: Maybe (Popup DialogChoice RName)
  , _tabSel       :: Tab
  }

emptyState :: State
emptyState = State
  { _focus        = Tracked
  , _tracked      = []
  , _trackedSel   = Nothing
  , _untracked    = []
  , _untrackedSel = Nothing
  , _bundles      = []
  , _bundleSel    = Nothing
  , _ignoreEdit   = editor Dotfiles Nothing ""
  , _error        = Nothing
  , _popup        = Nothing
  , _tabSel       = DotfileTab
  }

focusL :: Lens' State Focus
focusL = lens _focus (\s f -> s { _focus = f })

trackedL :: Lens' State [TrackedType]
trackedL = lens _tracked (\s ts -> s { _tracked = ts })

trackedSelL :: Lens' State (Maybe TrackedType)
trackedSelL = lens _trackedSel (\s t -> s { _trackedSel = t })

untrackedL :: Lens' State [FilePath]
untrackedL = lens _untracked (\s us -> s { _untracked = us })

untrackedSelL :: Lens' State (Maybe FilePath)
untrackedSelL = lens _untrackedSel (\s u -> s { _untrackedSel = u })

bundlesL :: Lens' State [Bundle]
bundlesL = lens _bundles (\s bs -> s { _bundles = bs })

bundleSelL :: Lens' State (Maybe Bundle)
bundleSelL = lens _bundleSel (\s b -> s { _bundleSel = b })

ignoreEditL :: Lens' State (Editor String RName)
ignoreEditL = lens _ignoreEdit (\s e -> s { _ignoreEdit = e })

errorL :: Lens' State (Maybe GitError)
errorL = lens _error (\s me -> s { _error = me })
