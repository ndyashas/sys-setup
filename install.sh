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
## This is a function defined in the `utils.sh` file.
setup_logging

## Start system setup.
print_section_start_message "system setup at $(date)"

## Install standard system packages.
print_section_start_message "system packages installation"
sys_pkgs_install


## Installing some system packages by custom methods
. ./sys-custom-packages-install.sh

print_section_start_message "system custom packages installation"
sys_custom_pkgs_install


## The following functions need to be run as the user
## rather than the root user.
su ${SUDO_USER} <<EOF

. ./utils.sh
. ./custom-packages-install.sh

## Install all the custom packages, preferably
## in the user's home directory
print_section_start_message "custom packages installation"
custom_pkgs_install

EOF
