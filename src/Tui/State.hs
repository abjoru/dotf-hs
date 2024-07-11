module Tui.State (
  RName(..),
  Focus(..),
  DialogChoice(..),
  Tab(..),
  State(..),

  emptyState,
  sampleState,
  hasFocus,
  countTracked,
  countUntracked,
  trackedSel,
  trackedSelFilePath,
  untrackedSel,

  focusL,
  trackedL,
  untrackedL,
  bundlesL,
  ignoreEditL,
  ignoreL,
  errorL,
  tabL,
  showAllTrackedL
) where

import           Brick.Widgets.Edit (Editor, editor)
import qualified Brick.Widgets.List as L
import           Data.Dotf          (Bundle, GitError,
                                     TrackedType (Staged, Tracked, Unstaged),
                                     trackedFile)
import qualified Data.Vector        as V
import           Lens.Micro         (Lens', lens, (^.))
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
  { _focus          :: Focus
  , _tracked        :: L.List RName TrackedType --[TrackedType]
  , _untracked      :: L.List RName FilePath
  , _bundles        :: L.List RName Bundle
  , _ignoreEdit     :: Editor String RName
  , _ignore         :: Bool
  , _error          :: Maybe GitError
  , _popup          :: Maybe (Popup DialogChoice RName)
  , _tab            :: Tab
  , _showAllTracked :: Bool
  }

emptyState :: State
emptyState = State
  { _focus          = FTracked
  , _tracked        = L.list RTrackedList V.empty 1
  , _untracked      = L.list RUntrackedList V.empty 1
  , _bundles        = L.list RBundleList V.empty 1
  , _ignoreEdit     = editor RIgnoreEditor Nothing ""
  , _ignore         = False
  , _error          = Nothing
  , _popup          = Nothing
  , _tab            = DotfileTab
  , _showAllTracked = False
  }

sampleState :: State
sampleState = emptyState
  { _tracked = L.list RTrackedList (V.fromList [ Tracked "/home/abjoru/.config/dotf/apt.yaml"
                                               , Tracked "/home/abjoru/.config/dotf/dotf.yaml"
                                               , Staged "/home/abjoru/.config/dotf/pacman.yaml"
                                               , Unstaged "/home/abjoru/.config/dotf/brew.yaml"
                                               ]) 1
  , _untracked = L.list RUntrackedList (V.fromList [ "/home/abjoru/.cache/nvim/ChatGPT.log"
                                                   , "/home/abjoru/.cache/nvim/fidget.nvim.log"
                                                   ]) 1
  }

hasFocus :: Focus -> State -> Bool
hasFocus expected st = expected == _focus st

countTracked :: State -> Int
countTracked s = V.length $ L.listElements $ _tracked s

countUntracked :: State -> Int
countUntracked s = V.length $ L.listElements $ _untracked s

trackedSel :: State -> Maybe TrackedType
trackedSel state =
  let l = state ^. trackedL
  in snd <$> L.listSelectedElement l

trackedSelFilePath :: State -> Maybe FilePath
trackedSelFilePath s = trackedFile <$> trackedSel s

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
focusL = lens _focus (\s f -> s { _focus = f })

trackedL :: Lens' State (L.List RName TrackedType)
trackedL = lens _tracked (\s ts -> s { _tracked = ts })

untrackedL :: Lens' State (L.List RName FilePath)
untrackedL = lens _untracked (\s us -> s { _untracked = us })

bundlesL :: Lens' State (L.List RName Bundle)
bundlesL = lens _bundles (\s bs -> s { _bundles = bs })

ignoreEditL :: Lens' State (Editor String RName)
ignoreEditL = lens _ignoreEdit (\s e -> s { _ignoreEdit = e })

ignoreL :: Lens' State Bool
ignoreL = lens _ignore (\s i -> s { _ignore = i })

errorL :: Lens' State (Maybe GitError)
errorL = lens _error (\s me -> s { _error = me })

tabL :: Lens' State Tab
tabL = lens _tab (\s t -> s { _tab = t })

showAllTrackedL :: Lens' State Bool
showAllTrackedL = lens _showAllTracked (\s t -> s { _showAllTracked = t })
