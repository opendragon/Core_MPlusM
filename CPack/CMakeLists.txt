#General Packaging Stuff
set(CPACK_CREATE_DESKTOP_LINKS "m+m manager")
set(CPACK_DISPLAY_NAME "Movement + Meaning")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "m+m Package")
set(CPACK_PACKAGE_EXECUTABLES "m+m manager" "m+m manager")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "m+m")
set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY_ "m+m")
set(CPACK_PACKAGE_NAME "m+m")
set(CPACK_PACKAGE_VERSION_MAJOR "${MpM_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${MpM_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${MpM_VERSION_PATCH}")
set(CPACK_RESOURCE_FILE_LICENSE "${MpM_SOURCE_DIR}/license.txt")
set(CPACK_RESOURCE_FILE_README "${MpM_SOURCE_DIR}/README.txt")
set(CPACK_VENDOR "H Plus Technologies Ltd. and Simon Fraser University")

#display name for our components
set(CPACK_COMPONENT_APPLICATIONS_DISPLAY_NAME "Standard Applications")
set(CPACK_COMPONENT_EXAMPLEAPPS_DISPLAY_NAME "Example Applications")
set(CPACK_COMPONENT_HEADERS_DISPLAY_NAME "C++ Header Files")
set(CPACK_COMPONENT_LIBRARIES_DISPLAY_NAME "m+m Libraries")
set(CPACK_COMPONENT_SOURCECODE_DISPLAY_NAME "Source Code")

#descriptions for our components
set(CPACK_COMPONENT_APPLICATIONS_DESCRIPTION "The executables that support running the m+m middleware")
set(CPACK_COMPONENT_EXAMPLEAPPS_DESCRIPTION "Examples of m+m applications")
set(CPACK_COMPONENT_HEADERS_DESCRIPTION "The header files required for new development")
set(CPACK_COMPONENT_LIBRARIES_DESCRIPTION "The fundamental files for an m+m installation")
set(CPACK_COMPONENT_SOURCECODE_DESCRIPTION
	"The source files for an m+m installation, required for extending or modifying the core applications or libraries")

#set up dependencies
set(CPACK_COMPONENT_APPLICATIONS_DEPENDS libraries)
set(CPACK_COMPONENT_EXAMPLEAPPS_DEPENDS libraries)
set(CPACK_COMPONENT_HEADERS_DEPENDS libraries)
set(CPACK_COMPONENT_SOURCECODE_DEPENDS libraries headers)

#set up CPack groups
set(CPACK_COMPONENT_APPLICATIONS_GROUP "Core")
set(CPACK_COMPONENT_EXAMPLEAPPS_GROUP "Optional")
set(CPACK_COMPONENT_HEADERS_GROUP "Core")
set(CPACK_COMPONENT_LIBRARIES_GROUP "Core")
set(CPACK_COMPONENT_SOURCECODE_GROUP "Optional")

#set up CPack constraints
set(CPACK_COMPONENT_LIBRARIES_REQUIRED ON)
set(CPACK_COMPONENT_EXAMPLEAPPS_DISABLED ON)
set(CPACK_COMPONENT_SOURCECODE_DISABLED ON)

#CPack group descriptions
set(CPACK_COMPONENT_GROUP_CORE_DESCRIPTION "The core m+m libraries and executables")
set(CPACK_COMPONENT_GROUP_CORE_BOLD_TITLE ON)
set(CPACK_COMPONENT_GROUP_OPTIONAL_DESCRIPTION
	"Example applications and source code for the libraries and standard applications")
	
set(CPACK_COMPONENTS_ALL applications exampleapps headers libraries sourcecode)

#end General Packaging stuff

# platform independent install step:
#
# TODO: copy our yarp headers to m+m install location/yarp
# TODO: copy our ace headers to m+m install location/ACE_wrappers
# NOTE: since we're not currently using CPack on mac, everything falls under WIN32 below:
#

#Windows specific install step
if(WIN32)

    set(CPACK_GENERATOR NSIS)

    install(FILES
            "${MpM_SOURCE_DIR}/bonjoursdksetup.exe"
            DESTINATION support
            COMPONENT libraries)

    # copy yarp and ACE DLL's to install location
    # set env variables for MpM_YARP_DIR and MpM_ACE_ROOT
    # so VS external, preconfigured projects can find them

    set(MpM_ACE_ROOT "${MpM_SOURCE_DIR}/../ACE_wrappers2")
    set(MpM_YARP_DIR "${MpM_SOURCE_DIR}/../yarp_MPlusM")

    #ACE HEADERS
    #install(DIRECTORY "${MpM_ACE_ROOT}/ace"
    #		DESTINATION ACE_wrappers
    #		COMPONENT libraries)

    #ACE DLL
    install(FILES
            "${MpM_ACE_ROOT}/lib/ACE.dll"
            DESTINATION bin
            COMPONENT libraries)
    #ACE LIB
    install(FILES
            "${MpM_ACE_ROOT}/lib/ACE.lib"
            DESTINATION lib
            COMPONENT libraries)

    #YARP HEADERS
    install(DIRECTORY "${MpM_YARP_DIR}/generated_include"
            DESTINATION yarp
            COMPONENT libraries)

    install(DIRECTORY "${MpM_YARP_DIR}/src/libYARP_OS/include"
            DESTINATION yarp/src/libYARP_OS/
            COMPONENT libraries)

    install(DIRECTORY "${MpM_YARP_DIR}/src/libYARP_name/include"
            DESTINATION yarp/src/libYARP_name/
            COMPONENT libraries)

    install(DIRECTORY "${MpM_YARP_DIR}/src/libYARP_dev/include"
            DESTINATION yarp/src/libYARP_dev/
            COMPONENT libraries)

    install(DIRECTORY "${MpM_YARP_DIR}/src/libYARP_math/include"
            DESTINATION yarp/src/libYARP_math/
            COMPONENT libraries)

    install(DIRECTORY "${MpM_YARP_DIR}/src/libYARP_sig/include"
            DESTINATION yarp/src/libYARP_sig/
            COMPONENT libraries)

    #YARP EXES
    install(FILES
            "${MpM_YARP_DIR}/bin/Release/yarp.exe"
            "${MpM_YARP_DIR}/bin/Release/yarpserver.exe"
            "${MpM_YARP_DIR}/bin/Release/yarpserver2.exe"
            "${MpM_YARP_DIR}/bin/Release/yarpserver3.exe"
            "${MpM_YARP_DIR}/bin/Release/yarp-config.exe"
            DESTINATION bin
            COMPONENT applications)

    #YARP LIBS
    install(FILES
            "${MpM_YARP_DIR}/lib/Release/YARP_OS.lib"
            "${MpM_YARP_DIR}/lib/Release/YARP_dev.lib"
            "${MpM_YARP_DIR}/lib/Release/YARP_name.lib"
            "${MpM_YARP_DIR}/lib/Release/YARP_init.lib"
            "${MpM_YARP_DIR}/lib/Release/YARP_sig.lib"
            DESTINATION lib
            COMPONENT libraries)

    #YARP DLLS
    install(FILES
            "${MpM_YARP_DIR}/lib/Release/YARP_OS.dll"
            "${MpM_YARP_DIR}/lib/Release/YARP_dev.dll"
            "${MpM_YARP_DIR}/lib/Release/YARP_name.dll"
            "${MpM_YARP_DIR}/lib/Release/YARP_init.dll"
            "${MpM_YARP_DIR}/lib/Release/YARP_sig.dll"
            DESTINATION bin
            COMPONENT libraries)

	#Configuration files
	install(FILES
			"${MpM_SOURCE_DIR}/standardApplications.txt"
			DESTINATION support
			COMPONENT libraries)

	#OTHER EXES
    install(FILES
            "${MpM_SOURCE_DIR}/../Utilities_mm_manager/Builds/VisualStudio2013/Release/m+m manager.exe"
            DESTINATION bin
            COMPONENT applications)

    #Source archives
    install(FILES
            "${MpM_SOURCE_DIR}/../archives/ACE_wrappers2.zip"
            "${MpM_SOURCE_DIR}/../archives/Core_MPlusM-master.zip"
            "${MpM_SOURCE_DIR}/../archives/OGDF.zip"
            "${MpM_SOURCE_DIR}/../archives/Utilities_mm_manager-master.zip"
            "${MpM_SOURCE_DIR}/../archives/Utilities_ServiceViewer-master.zip"
            "${MpM_SOURCE_DIR}/../archives/yarp_MPlusM-master.zip"
            DESTINATION src
            COMPONENT sourcecode)

    set(CPACK_NSIS_INSTALLED_ICON_NAME "bin/m+m manager.exe")
    set(CPACK_NSIS_MODIFY_PATH ON)
    set(CPACK_NSIS_MUI_ICON "${MpM_SOURCE_DIR}/InstallerIcon.ico")
    set(CPACK_NSIS_MUI_UNIICON "${MpM_SOURCE_DIR}/InstallerIcon.ico")
    set(CPACK_NSIS_URL_INFO_ABOUT "http://www.mplusm.ca")

    #Finally, set some MpM Environment variables (WINDOWS ONLY!)
    set(CPACK_NSIS_EXTRA_INSTALL_COMMANDS
        "
		CreateDirectory 'C:\\\\Users\\\\Public\\\\Documents\\\\m+m'
		CopyFiles '$INSTDIR/support/standardApplications.txt' 'C:\\\\Users\\\\Public\\\\Documents\\\\m+m'
		ExecWait '$INSTDIR/support/bonjoursdksetup.exe'
        WriteRegStr HKCU 'Environment' 'MPM_YARP_DIR' '$INSTDIR/yarp'
        WriteRegStr HKCU 'Environment' 'MPM_ACE_ROOT' '$INSTDIR/ACE_wrappers'
		
        Delete '$INSTDIR/support/bonjoursdksetup.exe'
		CreateShortCut \\\"$DESKTOP\\\\m+m manager.lnk\\\" \\\"$INSTDIR\\\\bin\\\\m+m manager.exe\\\"
		")

    #And, unset them when we uninstall!
    set(CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS
        "
		DeleteRegValue HKCU 'Environment' 'MPM_YARP_DIR'
        DeleteRegValue HKCU 'Environment' 'MPM_ACE_ROOT'
		Delete \\\"$DESKTOP\\\\m+m manager.lnk\\\"
		")

endif(WIN32)

include(CPack)
