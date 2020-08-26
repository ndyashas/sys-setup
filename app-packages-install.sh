#####################################################################
# This package runs the installation of various app packages        #
# the installation steps can get very specific to each package,     #
# thus, it is better to have separate functions for installing each #
# package.                                                          #
#####################################################################

## This array will hold all the functions. This array is used in the
## `app_pkgs_install()` function for internally invoking the user-supplied
## functions.
declare -a app_install_funcs_array

## It is desirable to have a directory in the user-home
## where all of the software files are located. The directory
## is defined in the ${TMP_SYS_INSTALLER_ROOT}/utils.sh
setting_up_software_root ()
{
    echo "NotImplementedError" 1>&2
    return 1
}

app_install_funcs_array=("${app_install_funcs_array[@]}" "setting_up_software_root")


app_pkgs_install()
{
    # Import the utils file.
    . ./utils.sh
    
    for app_install_function in "${app_install_funcs_array[@]}"
    do
        print_info_message "Running '${app_install_function}'" "${app_install_function}"
        "${app_install_function}" 1>>${LOG_FILE} 2>&1

        if ! [ "$?" = "0" ]; then
            print_error_message "Error running '${app_install_function}' in in $( basename "${BASH_SOURCE[0]}" )" "${app_install_function}"
        fi
    done
}
