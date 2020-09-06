###################################################################
# This script defines the `sys_pkg_install` function when this    #
# function is called, it installs/adds all the packages/ppa repos #
# listed in the ${TMP_SYS_INSTALLER_ROOT}/sys-packages-data.sh.   #
###################################################################

## Array containing packages that need to be installed.
declare -a pkgs_to_install=("wget"
                            "curl"
                            "git"
                            "emacs"
                            "screen"
                            "python3-pip")

## Array containing ppa repos that need to be added.
declare -a ppa_repos


## The main function for handling package installation in the system.
sys_pkgs_install()
{
    ## Import the required files.
    . ./utils.sh

    ## Check for `apt`, then update and upgrade the system.
    apt-get --version 1>/dev/null 2>/dev/null

    if ! [ "$?" = "0" ]; then
        print_error_message "Error encountered while testing for package apt" "apt"
        exit 1
    else
        print_info_message "Adding standard repositories"
        add-apt-repository main 1>>${LOG_FILE} 2>&1

        ## TODO: Define the repositories in an array. Possibly in
        ## the `sys-packages-data.sh` file?

        ## Uncomment the repository as when needed. Look at
        ## https://itsfoss.com/ubuntu-repositories/ for more details.

        add-apt-repository universe 1>>${LOG_FILE} 2>&1
        # add-apt-repository restricted 1>>${LOG_FILE} 2>&1
        # add-apt-repository multiverse 1>>${LOG_FILE} 2>&1

        print_info_message "Updating and upgrading system"
        apt-get update 1>>${LOG_FILE} 2>&1
        apt-get upgrade -y 1>>${LOG_FILE} 2>&1
    fi

    if ! [ "$?" = "0" ]; then
        print_error_message "Error encountered while updating and upgrading system"
        exit 1
    fi


    ## Adding any user supplied ppa reppositories.
    declare -a ppa_repos_failed_to_install

    for ppa_repo in "${ppa_repos[@]}"
    do
        print_info_message "Adding ppa repository ${ppa_repo}"
        echo add-apt-repository "${ppa_repo}" -y 1>>${LOG_FILE} 2>&1

        if ! [ "$?" = "0" ]; then
            print_error_message "Error encountered while adding ppa repository ${ppa_repo}" "${ppa_repo}"
            ppa_repos_failed_to_install=("${ppa_repos_failed_to_install[@]}" "${ppa_rep}")
        fi
    done


    ## Install required packages.
    declare -a pkgs_failed_to_install

    for pkg in "${pkgs_to_install[@]}"
    do
        print_info_message "Installing package ${pkg}" "${pkg}"
        apt-get install "${pkg}" -y 1>>${LOG_FILE} 2>&1

        if ! [ "$?" = "0" ]; then
            print_error_message "Error encountered while installing package ${pkg}" "${pkg}"
            pkgs_failed_to_install=("${pkgs_failed_to_install[@]}" "${pkg}")
    fi
    done


    ## Cleanup after all package installation.
    print_info_message "Removing unwanted packages that are not used"
    apt-get autoremove -y 1>>${LOG_FILE} 2>&1
}
