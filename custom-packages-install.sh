#####################################################################
# This script runs the installation of various custom packages      #
# the installation steps can get very specific to each package,     #
# thus, it is better to have separate functions for installing each #
# package.                                                          #
#####################################################################

## This array will hold all the functions. This array is used in the
## `custom_pkgs_install()` function for internally invoking the user-supplied
## functions.
declare -a custom_install_funcs_array

python_virtual_environment_setup ()
{
    pip3 install virtualenv virtualenvwrapper 1>>${LOG_FILE} 2>&1
    return "$?"
}

custom_install_funcs_array=("${custom_install_funcs_array[@]}" "python_virtual_environment_setup")


dotbot_dispatch ()
{
    dotfiles/install 1>>${LOG_FILE} 2>&1
    return "$?"
}

custom_install_funcs_array=("${custom_install_funcs_array[@]}" "dotbot_dispatch")


custom_pkgs_install()
{
    # Import the utils file.
    . ./utils.sh
    
    for custom_install_function in "${custom_install_funcs_array[@]}"
    do
        print_info_message "Running '${custom_install_function}'" "${custom_install_function}"
        "${custom_install_function}" 1>>${LOG_FILE} 2>&1

        if ! [ "$?" = "0" ]; then
            print_error_message "Error running '${custom_install_function}' in in $( basename "${BASH_SOURCE[0]}" )" "${custom_install_function}"
        fi
    done
}
