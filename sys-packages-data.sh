################################################################################
# This file more like a config file which defines the packages, ppa repos that #
# need to be installed/added to the system.                                    #
################################################################################

## Array containing packages that need to be installed.
declare -a pkgs_to_install=("wget"
                            "curl"
                            "git"
                            "emacs"
                            "zsh")

## Array containing ppa repos that need to be added.
declare -a ppa_repos
