# dotf-hs

![Version](https://img.shields.io/badge/version-1.0.1.0-blue)

`dotf-hs` is a Haskell implementation of the dotfiles workflow I use on Linux and macOS. It keeps a bare Git repository in `$HOME/.dotf`, installs operating-system packages declared in YAML bundles, and provides a Brick/Vty TUI for reviewing tracked/untracked files, XMonad launchers, and bundle definitions.

## Features
- Wraps the bare-repo pattern so `dotf install|update|commit|push|pull` always target `$HOME` as the work-tree while storing Git data in `$HOME/.dotf`.
- Bundle system (`~/.config/dotf/bundles/*.yaml`) that lists OS packages, Git checkouts (with optional install commands), and pre/post-install scripts. Bundles can be marked as `headless` so you can skip desktop-only software on servers.
- TUI (`dotf` with no args) that exposes three tabs:
  - **Dotfiles** – inspect tracked/staged/unstaged files, add/remove/ignore paths, run diffs, and commit without leaving the terminal.
  - **Bundles** – browse bundle definitions, filter packages, and scaffold new bundles from a template.
  - **XMonad** – visualize app launchers defined in `~/.config/xmonad/applications.yaml`.
- CLI conveniences: `--dryrun` flag echoes the Git/package commands without executing; `--headless` installs only bundles flagged as headless.

## Getting Started

### 1. Build the project
1. Clone the repo and install the Haskell toolchain (Stack or Cabal):
   ```bash
   git clone https://github.com/abjoru/dotf-hs.git
   cd dotf-hs
   stack build            # or: cabal build
   ```
2. Run tests if you are hacking on the project:
   ```bash
   stack test             # or: cabal test
   ```

### 2. Bootstrap the dotfiles repo
`dotf-hs` expects to work against a bare repo in `$HOME/.dotf`. The binary can either clone an existing remote or create one locally:

```bash
# Clone an existing dotfiles repository
stack run dotf -- init git@github.com:<user>/<repo>.git

# OR create a brand new bare repo in ~/.dotf
stack run dotf -- new
```

During the first execution `dotf` checks for required package managers (`paru` for Arch, `brew` for macOS, `apt` for Debian) and offers to install them.

### 3. Describe your system with bundles
Bundle files live in `~/.config/dotf/bundles/` and look like this:

```yaml
name: desktop
headless: false
pre-install: scripts/desktop/preinstall.sh
post-install: scripts/desktop/postinstall.sh

os-packages:
  - firefox:
      arch: firefox
      osx: firefox
  - neovim:
      arch: neovim
      osx: neovim

git-packages:
  - name: my-tooling
    url: https://github.com/me/my-tooling.git
    install-cmd: stack install
```

Use the `Bundles` tab in the TUI or the helper that appears when you press `n` to scaffold a new bundle. Paths inside bundle files are resolved relative to `~/.config/dotf/`.

### 4. Install or update packages
Once you have bundle files in place, run:

```bash
stack run dotf -- install        # install OS packages, clone Git repos, run scripts
stack run dotf -- update         # rerun installers for what is already present
stack run dotf -- install --dryrun --headless   # preview headless install commands
```

### 5. Use the TUI and Git helpers
Running `dotf` without arguments launches the TUI if `~/.dotf` exists; otherwise you will be shown the bootstrap instructions. From the CLI you can also run:

```bash
stack run dotf -- status
stack run dotf -- diff
stack run dotf -- commit "Update configs"
stack run dotf -- push
stack run dotf -- pull
stack run dotf -- git "<any raw git args>"
```

All commands automatically target your bare repo/work tree so you never have to remember the `--git-dir`/`--work-tree` flags manually.

## Development Notes
- The project targets GHC via Stack (see `stack.yaml`) but also ships a Cabal file (`dotf-hs.cabal`); use whichever workflow you prefer.
- Source lives in `src/` (CLI + bundle logic under `Dotf.*`, TUI under `Tui.*`), with integration tests in `test/`.
- Pull requests that add bundle helpers or improve the TUI are welcome; please run `stack test` before submitting.
