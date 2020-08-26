#########################################################################
# This file contains the global variables for logging, install-dir etc. #
# additionally, information and error logging wrappers are also defined #
# in this file.                                                         #
#########################################################################


## The directory where all the log files will be generated.
LOG_DIR=/tmp/sys-setup/log
LOG_FILE=${LOG_DIR}/sys-setup.log

## Terminal colors.
ERROR_COLOR='\033[0;31m'
INFO_COLOR='\033[1;32m'
INFO_COLOR_2='\033[1;33m'
SECTION_HEADER_COLOR='\033[1;34m'
NO_COLOR='\033[0m'

## Colored prefix for error and information messages.
ERROR_MSG=${ERROR_COLOR}"[ERROR]"${NO_COLOR}
INFO_MSG=${INFO_COLOR}"[INFO]"${NO_COLOR}


## Function for setting up the infrastructre for logging messages.
setup_logging()
{
    mkdir -p ${LOG_DIR}
    touch ${LOG_FILE}
    chown -R ${SUDO_USER} ${LOG_DIR}
}

## Wrapper for the echo message which adds colored prefix to info-messages.
## First argument is the info-message.
## Second argument is the sub-string in the first argument which
## needs to be highlighted.
print_info_message()
{
    echo -e "[INFO]  ${1}" 1>>${LOG_FILE}

    # Colorise the message.
    to_print="${INFO_MSG}  ${1}"
    if [ -n "$2" ]; then
        to_print="${to_print//$2/${INFO_COLOR_2}$2${NO_COLOR}}"
    fi

    echo -e "${to_print}"
    return 0
}

## Wrapper for the echo message which adds colored prefix to error-messages.
## First argument is the error-message.
## Second argument is the sub-string in the first argument which
## needs to be highlighted.
print_error_message()
{

    echo -e "[ERROR] ${1}\n" 1>>${LOG_FILE}

    # Colorise the message.
    to_print="${ERROR_MSG} ${1}"
    if [ -n "$2" ]; then
        to_print="${to_print//$2/${INFO_COLOR_2}$2${NO_COLOR}}"
    fi

    echo -e "${to_print}. Please see the log file at ${LOG_FILE} for more information.\n" 1>&2
    return 0
}

## Wrapper for the echo message which is displayed at the starting of a phase
print_section_start_message()
{
    to_print="\n[NEW]   Starting section - ${1}"
    echo -e "${to_print}" 1>>${LOG_FILE}

    # Colorise the message.
    to_print="${to_print/\[NEW\]/${SECTION_HEADER_COLOR}[NEW]${NO_COLOR}}"
    to_print="${to_print//${1}/${SECTION_HEADER_COLOR}${1}${NO_COLOR}}"

    echo -e "${to_print}"
    return 0
}
