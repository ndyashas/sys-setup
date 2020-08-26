#!/bin/bash

##################################################################
# This is the main script for setting up the system. This script #
# invokes various sub-functions for system package installation, #
# dotfile configuration etc.                                     #
##################################################################


## Check if the user is running the script with sudo previlages.
if ! [ "$(id -u)" = "0" ]; then
    echo "The script needs to be run with root previlages" 1>&2
    exit 1
fi

## Import all the library functions.
. ./utils.sh
. ./sys-packages-install.sh

## Setup logging infrastructure.
## This is a function defined in the `utild.sh` file.
setup_logging

## Start system setup.
print_info_message "Starting system setup at $(date)"

# Install all the system packages.
sys_pkgs_install