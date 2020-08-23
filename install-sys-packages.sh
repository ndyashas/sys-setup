#!/bin/sh

# Follows semantic versioning
VERSION="0.0.1"

# The directory where all the log files will be generated
LOG_DIR=/tmp/sys-setup-"${VERSION}"/log
LOG_FILE=${LOG_DIR}/sys-setup-$(date +%d-%m-%Y-%M-%S).log


## Check if the user is running the script with sudo previlages
if ! [ "$(id -u)" = "0" ]; then
	echo "[ERROR] The script needs to be run as root." >&2
	exit 1
fi


## Make a log directory and a log file
mkdir -p ${LOG_DIR}
touch ${LOG_FILE}
chown -R ${SUDO_USER} ${LOG_DIR}


# Start system setup
echo "Starting system setup at $(date)" | tee -a ${LOG_FILE}
