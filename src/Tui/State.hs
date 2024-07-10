module Tui.State (
  RName,
  Focus(..),
  DialogChoice(..),
  Tab(..),
  State(..),

  emptyState,
  sampleState,
  hasFocus,

  focusL,
  trackedL,
  untrackedL,
  bundlesL,
  ignoreEditL,
  errorL,
  tabL
) where

import           Brick.Widgets.Edit (Editor, editor)
import qualified Brick.Widgets.List as L
import           Data.Dotf          (Bundle, GitError,
                                     TrackedType (Staged, Tracked, Unstaged))
import qualified Data.Vector        as V
import           Lens.Micro         (Lens', lens)
import           Tui.Popup          (Popup)

data RName
  = RTrackedList
  | RUntrackedList
  | RBundleList
  | RIgnoreEditor
  deriving (Eq, Ord, Show)

data Focus
  = FTracked
  | FUntracked
  | FIgnoreDialog
  | FHelpDialog
  | FErrorDialog
  | FBundleList
  | FPackageTable
  | FGitPackageTable
  | FScriptList
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
  , _tab        :: Tab
  }

emptyState :: State
emptyState = State
  { _focus        = FTracked
  , _tracked      = L.list RTrackedList V.empty 1
  , _untracked    = L.list RUntrackedList V.empty 1
  , _bundles      = L.list RBundleList V.empty 1
  , _ignoreEdit   = editor RIgnoreEditor Nothing ""
  , _error        = Nothing
  , _popup        = Nothing
  , _tab          = DotfileTab
  }

sampleState :: State
sampleState = emptyState
  { _tracked = L.list RTrackedList (V.fromList [ Tracked "~/.config/dotf/apt.yaml"
                                               , Tracked "~/.config/dotf/dotf.yaml"
                                               , Staged "~/.config/dotf/pacman.yaml"
                                               , Unstaged "~/.config/dotf/brew.yaml"
                                               ]) 1
  , _untracked = L.list RUntrackedList (V.fromList [ "~/.cache/nvim/ChatGPT.log"
                                                   , "~/.cache/nvim/fidget.nvim.log"
                                                   ]) 1
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

--trackedSelL :: Lens' State (Maybe FilePath)
--trackedSelL = 

untrackedL :: Lens' State (L.List RName FilePath)
untrackedL = lens _untracked (\s us -> s { _untracked = us })

bundlesL :: Lens' State (L.List RName Bundle)
bundlesL = lens _bundles (\s bs -> s { _bundles = bs })

ignoreEditL :: Lens' State (Editor String RName)
ignoreEditL = lens _ignoreEdit (\s e -> s { _ignoreEdit = e })

errorL :: Lens' State (Maybe GitError)
errorL = lens _error (\s me -> s { _error = me })

tabL :: Lens' State Tab
tabL = lens _tab (\s t -> s { _tab = t })
