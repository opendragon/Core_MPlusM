#General Packaging Stuff
set(CPACK_PACKAGE_NAME "M+M")
set(CPACK_VENDOR "Movement and Meaning")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "M+M Package")
set(CPACK_PACKAGE_VERSION "1.0.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")
set(CPACK_PACKAGE_VERSION_PATCH "0")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "M+M")
set(CPACK_RESOURCE_FILE_LICENSE ${MpM_SOURCE_DIR}/license.txt)
#display name for our components
set(CPACK_COMPONENT_APPLICATIONS_DISPLAY_NAME "Core Applications")
set(CPACK_COMPONENT_EXAMPLES_DISPLAY_NAME "Example Applications")
set(CPACK_COMPONENT_LIBRARIES_DISPLAY_NAME "M+M Libraries")
set(CPACK_COMPONENT_HEADERS_DISPLAY_NAME "C++ Header Files")
#set up dependencies
set(CPACK_COMPONENT_HEADERS_DEPENDS libraries)
set(CPACK_COMPONENT_APPLICATIONS_DEPENDS libraries)
set(CPACK_COMPONENT_EXAMPLES_DEPENDS libraries)
#set up CPack groups
set(CPACK_COMPONENT_LIBRARIES_GROUP "Minimal")
set(CPACK_COMPONENT_APPLICATIONS_GROUP "Minimal")
set(CPACK_COMPONENT_HEADERS_GROUP "Minimal")
set(CPACK_COMPONENT_EXAMPLES_GROUP "Extra")
set(CPACK_COMPONENT_YARP_GROUP "Extra")
#CPack group descriptions
set(CPACK_COMPONENT_GROUP_MINIMAL_DESCRIPTION
	"Just the M+M libraries and core executables")
set(CPACK_COMPONENT_GROUP_EVERYTHING_DESCRIPTION
	"Everything, including sample M+M Applications")
	
set(CPACK_COMPONENTS_ALL applications libraries headers examples)

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

install(FILES "${MpM_SOURCE_DIR}/bonjoursdksetup.exe"
			DESTINATION support
			COMPONENT libraries)


# copy yarp and ACE DLL's to install location
# set env variables for MpM_YARP_DIR and MpM_ACE_ROOT
# so VS external, preconfigured projects can find them

set(MpM_ACE_ROOT "${MpM_SOURCE_DIR}/../ACE_wrappers")
set(MpM_YARP_DIR "${MpM_SOURCE_DIR}/../yarp")

#ACE HEADERS
#install(DIRECTORY "${MpM_ACE_ROOT}/ace"
#		DESTINATION ACE_wrappers
#		COMPONENT libraries)

#ACE DLL
install(FILES 	"${MpM_ACE_ROOT}/lib/ACE.dll"
            DESTINATION bin
            COMPONENT libraries)
#ACE LIB
install(FILES 	"${MpM_ACE_ROOT}/lib/ACE.lib"
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
install(FILES 	"${MpM_YARP_DIR}/bin/Release/yarp.exe"
				"${MpM_YARP_DIR}/bin/Release/yarpserver.exe"
				"${MpM_YARP_DIR}/bin/Release/yarpserver2.exe"
				"${MpM_YARP_DIR}/bin/Release/yarpserver3.exe"
				"${MpM_YARP_DIR}/bin/Release/yarp-config.exe"
            DESTINATION bin
            COMPONENT applications)
		
#YARP LIBS
install(FILES 	"${MpM_YARP_DIR}/lib/Release/YARP_OS.lib"
				"${MpM_YARP_DIR}/lib/Release/YARP_dev.lib"
				"${MpM_YARP_DIR}/lib/Release/YARP_name.lib"
				"${MpM_YARP_DIR}/lib/Release/YARP_init.lib"
				"${MpM_YARP_DIR}/lib/Release/YARP_sig.lib"
            DESTINATION lib
            COMPONENT libraries)

#YARP DLLS
install(FILES 	"${MpM_YARP_DIR}/lib/Release/YARP_OS.dll"
				"${MpM_YARP_DIR}/lib/Release/YARP_dev.dll"
				"${MpM_YARP_DIR}/lib/Release/YARP_name.dll"
				"${MpM_YARP_DIR}/lib/Release/YARP_init.dll"
				"${MpM_YARP_DIR}/lib/Release/YARP_sig.dll"
            DESTINATION bin
            COMPONENT libraries)
			
#OTHER EXES
install(FILES 	"${MpM_SOURCE_DIR}/../Utilities/ChannelManager/Builds/VisualStudio2012/Release/Channel Manager.exe"
        DESTINATION bin
        COMPONENT applications)


			

#Finally, set some MpM Env variables (WINDOWS ONLY!)
set(CPACK_NSIS_EXTRA_INSTALL_COMMANDS " 
					ExecWait '$INSTDIR/support/bonjoursdksetup.exe'
					WriteRegStr HKCU 'Environment' 'MPM_YARP_DIR' '$INSTDIR\\yarp'
					WriteRegStr HKCU 'Environment' 'MPM_ACE_ROOT' '$INSTDIR\\ACE_wrappers'
					Delete '$INSTDIR/support/bonjoursdksetup.exe'
					")		
#And, unset them when we uninstall!
set(CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS " 
					DeleteRegValue HKCU 'Environment' 'MPM_YARP_DIR'
					DeleteRegValue HKCU 'Environment' 'MPM_ACE_ROOT'
					")						
endif(WIN32)

include(CPack)