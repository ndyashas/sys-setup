#####################################################################
# This package runs the installation of various custom packages     #
# the installation steps can get very specific to each package,     #
# thus, it is better to have separate functions for installing each #
# package.                                                          #
#####################################################################

## This array will hold all the functions. This array is used in the
## `sys_custom_pkgs_install()` function for internally invoking the user-supplied
## functions.
declare -a sys_custom_install_funcs_array

dummy_func ()
{
    return 0
}

sys_custom_install_funcs_array=("${custom_install_funcs_array[@]}" "dummy_func")


sys_custom_pkgs_install()
{
    # Import the utils file.
    . ./utils.sh
    
    for sys_custom_install_function in "${sys_custom_install_funcs_array[@]}"
    do
        print_info_message "Running '${sys_custom_install_function}'" "${sys_custom_install_function}"
        "${sys_custom_install_function}" 1>>${LOG_FILE} 2>&1

        if ! [ "$?" = "0" ]; then
            print_error_message "Error running '${sys_custom_install_function}' in in $( basename "${BASH_SOURCE[0]}" )" "${sys_custom_install_function}"
        fi
    done
}
