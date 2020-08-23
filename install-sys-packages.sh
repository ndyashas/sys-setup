#!/bin/sh

## Check if the user is running the script with sudo previlages
if ! [ "$(id -u)" = "0" ]; then
	echo "The script needs to be run with root previlages"
	exit 1
fi

# Import the error and info loggin functions
. ./utils.sh


# Setup logging infrastructure
setup_logging


# Start system setup
print_info_message "Starting system setup at $(date)"


# Check for `apt`, then update and upgrade the system
apt-get --version 1>/dev/null 2>/dev/null

if ! [ "$?" = "0" ]; then
	print_error_message "Error encountered while testing for package apt" "apt"
	exit 1
else
	print_info_message "Updating and upgrading system"
	apt-get update 1>>${LOG_FILE} 2>&1
	apt-get upgrade -y 1>>${LOG_FILE} 2>&1
fi

if ! [ "$?" = "0" ]; then
	print_error_message "Error encountered while updating and upgrading system"
	exit 1
fi


# Install required packages
declare -a pkgs_to_install=("git"
                            "emacs")

declare -a pkgs_failed_to_install

for pkg in "${pkgs_to_install[@]}"
do
	print_info_message "Installing package ${pkg}" "${pkg}"
	apt-get install "${pkg}" -y 1>>${LOG_FILE} 2>&1

	if ! [ "$?" = "0" ]; then
		print_error_message "Error encountered while installing package ${pkg}" "${pkg}"
		pkgs_failed_to_install+=("${pkg}")
	fi
done
