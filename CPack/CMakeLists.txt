#General Packaging Stuff
set(CPACK_PACKAGE_NAME "M+M")
set(CPACK_VENDOR "Movement and Meaning")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "M+M Test Package")
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
include(CPack)
#end General Packaging stuff

# platform independent install step:
#
# TODO: copy our yarp folder to M+M install location/yarp
# TODO: copy our ace folder to M+M install location/ACE_wrappers
#

#Windows specific install step
if(WIN32)
# copy yarp and ACE DLL's to install location
# set env variables for MpM_YARP_DIR and MpM_ACE_ROOT
# so VS external, preconfigured projects can find them
install(DIRECTORY ${MpM_SOURCE_DIR}/../yarp/generated_include 
		DESTINATION yarp 
		COMPONENT libraries)
		
install(DIRECTORY ${MpM_SOURCE_DIR}/../yarp/src/libYARP_OS/include
		DESTINATION yarp/src/libYARP_OS/
		COMPONENT libraries)

install(DIRECTORY ${MpM_SOURCE_DIR}/../yarp/src/libYARP_name/include
		DESTINATION yarp/src/libYARP_name/
		COMPONENT libraries)

install(DIRECTORY ${MpM_SOURCE_DIR}/../yarp/src/libYARP_dev/include
		DESTINATION yarp/src/libYARP_dev/
		COMPONENT libraries)

install(DIRECTORY ${MpM_SOURCE_DIR}/../yarp/src/libYARP_math/include
		DESTINATION yarp/src/libYARP_math/
		COMPONENT libraries)

install(DIRECTORY ${MpM_SOURCE_DIR}/../yarp/src/libYARP_sig/include
		DESTINATION yarp/src/libYARP_sig/
		COMPONENT libraries)		
endif(WIN32)