#General Packaging Stuff
set(CPACK_CREATE_DESKTOP_LINKS "Channel Manager")
set(CPACK_DISPLAY_NAME "Movement + Meaning")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "M+M Package")
set(CPACK_PACKAGE_EXECUTABLES "Channel Manager" "Channel Manager")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "M+M")
set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY "M+M")
set(CPACK_PACKAGE_NAME "M+M")
set(CPACK_PACKAGE_VERSION_MAJOR "${MpM_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${MpM_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${MpM_VERSION_PATCH}")
set(CPACK_RESOURCE_FILE_LICENSE "${MpM_SOURCE_DIR}/license.txt")
set(CPACK_RESOURCE_FILE_README "${MpM_SOURCE_DIR}/README.txt")
set(CPACK_VENDOR "H Plus Technologies Ltd. and Simon Fraser University")

#display name for our components
set(CPACK_COMPONENT_APPLICATIONS_DISPLAY_NAME "Core Applications")
set(CPACK_COMPONENT_EXAMPLES_DISPLAY_NAME "Example Applications")
set(CPACK_COMPONENT_HEADERS_DISPLAY_NAME "C++ Header Files")
set(CPACK_COMPONENT_LIBRARIES_DISPLAY_NAME "M+M Libraries")
set(CPACK_COMPONENT_SOURCE_DISPLAY_NAME "Source code")

#set up dependencies
set(CPACK_COMPONENT_APPLICATIONS_DEPENDS libraries)
set(CPACK_COMPONENT_EXAMPLES_DEPENDS libraries)
set(CPACK_COMPONENT_HEADERS_DEPENDS libraries)
set(CPACK_COMPONENT_SOURCE_DEPENDS libraries)

#set up CPack groups
set(CPACK_COMPONENT_APPLICATIONS_GROUP "Minimal")
set(CPACK_COMPONENT_EXAMPLES_GROUP "Extra")
set(CPACK_COMPONENT_HEADERS_GROUP "Minimal")
set(CPACK_COMPONENT_LIBRARIES_GROUP "Minimal")
set(CPACK_COMPONENT_SOURCE_GROUP "Extra")

#CPack group descriptions
set(CPACK_COMPONENT_GROUP_MINIMAL_DESCRIPTION
	"Just the M+M libraries and core executables")
set(CPACK_COMPONENT_GROUP_EVERYTHING_DESCRIPTION
	"Everything, including sample M+M Applications")
	
set(CPACK_COMPONENTS_ALL applications examples headers libraries source)

#end General Packaging stuff

# platform independent install step:
#
# TODO: copy our yarp headers to M+M install location/yarp
# TODO: copy our ace headers to M+M install location/ACE_wrappers
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

    #OTHER EXES
    install(FILES
            "${MpM_SOURCE_DIR}/../Utilities_ChannelManager/Builds/VisualStudio2013/Release/Channel Manager.exe"
            DESTINATION bin
            COMPONENT applications)

    #Source archives
    install(FILES
            "${MpM_SOURCE_DIR}/../archives/ACE_wrappers2.zip"
            "${MpM_SOURCE_DIR}/../archives/Core_MPlusM-master.zip"
            "${MpM_SOURCE_DIR}/../archives/OGDF.zip"
            "${MpM_SOURCE_DIR}/../archives/Utilities_ChannelManager-master.zip"
            "${MpM_SOURCE_DIR}/../archives/Utilities_ServiceViewer-master.zip"
            "${MpM_SOURCE_DIR}/../archives/yarp_MPlusM-master.zip"
            DESTINATION src
            COMPONENT source)

    set(CPACK_NSIS_INSTALLED_ICON_NAME "bin/Channel Manager.exe")
    set(CPACK_NSIS_MODIFY_PATH ON)
    set(CPACK_NSIS_MUI_ICON "${MpM_SOURCE_DIR}/InstallerIcon.ico")
    set(CPACK_NSIS_MUI_UNIICON "${MpM_SOURCE_DIR}/InstallerIcon.ico")
    set(CPACK_NSIS_URL_INFO_ABOUT "http://www.mplusm.ca")

    #Finally, set some MpM Environment variables (WINDOWS ONLY!)
    set(CPACK_NSIS_EXTRA_INSTALL_COMMANDS
        "
		ExecWait '$INSTDIR/support/bonjoursdksetup.exe'
        WriteRegStr HKCU 'Environment' 'MPM_YARP_DIR' '$INSTDIR/yarp'
        WriteRegStr HKCU 'Environment' 'MPM_ACE_ROOT' '$INSTDIR/ACE_wrappers'
        Delete '$INSTDIR/support/bonjoursdksetup.exe'
		CreateShortCut \\\"$DESKTOP\\\\Channel Manager.lnk\\\" \\\"$INSTDIR\\\\bin\\\\Channel Manager.exe\\\"
		")

    #And, unset them when we uninstall!
    set(CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS
        "
		DeleteRegValue HKCU 'Environment' 'MPM_YARP_DIR'
        DeleteRegValue HKCU 'Environment' 'MPM_ACE_ROOT'
		Delete \\\"$DESKTOP\\\\Channel Manager.lnk\\\"
		")

endif(WIN32)

include(CPack)
