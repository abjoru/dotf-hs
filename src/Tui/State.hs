module Tui.State (
  RName,
  Focus(..),
  DialogChoice(..),
  Tab(..),
  State(..),

  emptyState,
  hasFocus,

  focusL,
  trackedL,
  untrackedL,
  bundlesL,
  ignoreEditL,
  errorL
) where

import           Brick.Widgets.Edit (Editor, editor)
import qualified Brick.Widgets.List as L
import           Data.Dotf          (Bundle, GitError, TrackedType)
import           Data.Vector        as V
import           Lens.Micro         (Lens', lens)
import           Tui.Popup          (Popup)

-- Not sure about this one?
-- Do we track specific resources?
-- Would a tab be a viewport in this sense?
data RName
  = RTrackedList
  | RUntrackedList
  | RBundleList
  | RIgnoreEditor
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
  { _focus      :: Focus
  , _tracked    :: L.List RName TrackedType --[TrackedType]
  , _untracked  :: L.List RName FilePath
  , _bundles    :: L.List RName Bundle
  , _ignoreEdit :: Editor String RName
  , _error      :: Maybe GitError
  , _popup      :: Maybe (Popup DialogChoice RName)
  , _tabSel     :: Tab
  }

emptyState :: State
emptyState = State
  { _focus        = Tracked
  , _tracked      = L.list RTrackedList V.empty 1
  , _untracked    = L.list RUntrackedList V.empty 1
  , _bundles      = L.list RBundleList V.empty 1
  , _ignoreEdit   = editor RIgnoreEditor Nothing ""
  , _error        = Nothing
  , _popup        = Nothing
  , _tabSel       = DotfileTab
  }

hasFocus :: Focus -> State -> Bool
hasFocus expected st = expected == _focus st

------------
-- Lenses --
------------

focusL :: Lens' State Focus
focusL = lens _focus (\s f -> s { _focus = f })

trackedL :: Lens' State (L.List RName TrackedType)
trackedL = lens _tracked (\s ts -> s { _tracked = ts })

untrackedL :: Lens' State (L.List RName FilePath)
untrackedL = lens _untracked (\s us -> s { _untracked = us })

bundlesL :: Lens' State (L.List RName Bundle)
bundlesL = lens _bundles (\s bs -> s { _bundles = bs })

ignoreEditL :: Lens' State (Editor String RName)
ignoreEditL = lens _ignoreEdit (\s e -> s { _ignoreEdit = e })

errorL :: Lens' State (Maybe GitError)
errorL = lens _error (\s me -> s { _error = me })
