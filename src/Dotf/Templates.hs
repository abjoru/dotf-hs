module Dotf.Templates (
  bundleFileTemplate,
) where

import           Data.String.Interpolate (__i)

bundleFileTemplate :: String -> String
bundleFileTemplate name =
  [__i|\# Sample bundle file
  name: "#{name}"
  \# [Optional] Whether X libs are required (default: true)
  \#headless: false
  \# [Optional] script to be run prior to bundle install.
  \# Note: Base for paths is ~/.config/dotf/
  \#pre-install: scripts/#{name}/preinstall.sh
  \# [Optional] script to be run after bundle install.
  \# Note: Base for paths is ~/.config/dotf/
  \#post-install: scripts/#{name}/postinstall.sh

  \# Packages for this bundle defined as OS specific
  \# managed packages. There are currently 3 package
  \# managers supported by DotF:
  \# - paru (Arch Linux)
  \# - homebrew (MacOSX)
  \# - apt (Debian Linux)
  os-packages:
    \# Defines a package. Name can be anything you want as
    \# this is just a container for package managed variants.
    \# Note that of a given package manager is left off the list,
    \# it will be considered not needed for that particular OS.
    \#- mypackagename:
        \# [Optional] Defines the pacman/paru package name if any
        \#arch: arch-packagename
        \# [Optional] Defines the homebrew package name if any
        \#osx: osx-packagename
        \# [Optional] Defines the deb package name if any
        \#deb: deb-packagename

  \# Optional section for GIT related packages to be built.
  \#git-packages:
    \# Each package starts with a name
    \#- name: my-git-package
      \# Each package needs a URL
      \#url: "https://github.com/my/git/package"
      \# [Optional] A package may define a target branch
      \#branch: master
      \# [Optional] A package may want to fetch submodules (default: false)
      \#submodules: false
      \# [Optional] A package may have an install command
      \#install-cmd: "stack install"|]
