# This file contains the global variables and the util functions

# The directory where all the log files will be generated
LOG_DIR=/tmp/sys-setup/log
LOG_FILE=${LOG_DIR}/sys-setup.log

ERROR_COLOR='\033[0;31m'
INFO_COLOR='\033[1;32m'
INFO_COLOR_2='\033[1;33m'
NO_COLOR='\033[0m'

ERROR_MSG=${ERROR_COLOR}"[ERROR]"${NO_COLOR}
INFO_MSG=${INFO_COLOR}"[INFO]"${NO_COLOR}


setup_logging()
{
	mkdir -p ${LOG_DIR}
	touch ${LOG_FILE}
	chown -R ${SUDO_USER} ${LOG_DIR}
}

print_info_message()
{
	echo -e "[INFO]  ${1}" 1>>${LOG_FILE}

	# Colorise the message
	to_print="${INFO_MSG} $1"
	if [ -n $2 ]; then
		to_print="${to_print/$2/${INFO_COLOR_2}$2${NO_COLOR}}"
	fi

	echo -e ${to_print}
	return 0
}

print_error_message()
{

	echo -e "[ERROR] ${1}\n" 1>>${LOG_FILE}

	# Colorise the message
	to_print="${ERROR_MSG} $1"
	if [ -n $2 ]; then
		to_print="${to_print/$2/${INFO_COLOR_2}$2${NO_COLOR}}"
	fi

	echo -e ${to_print} 1>&2
	echo -e "Please see the log file at ${LOG_FILE} for more information.\n" 1>&2
	return 0
}
