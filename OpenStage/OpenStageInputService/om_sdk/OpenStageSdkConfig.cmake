# ==============================================================================
#  Copyright(c) 2011-2012 Organic Motion, Inc. All Rights Reserved.
#
#  The coded instructions, statements, computer programs, and/or related
#  material (collectively the "Code") in these files contain unpublished
#  information proprietary to Organic Motion, Inc., which is protected by
#  United States of America federal copyright law and by international
#  treaties. Use, duplication, and or distribution only with written
#  permission from Organic Motion, Inc.
#
#  THE CODE IS PROVIDED "AS IS" AND WITHOUT WARRANTY.
# ==============================================================================

get_filename_component(OPENSTAGESDK_DIR ${CMAKE_CURRENT_LIST_FILE} PATH)

find_path(OPENSTAGESDK_INCLUDE_DIR om/sdk2/client.h HINTS ${OPENSTAGESDK_DIR}/include)

find_library(OPENSTAGESDK_LIBRARY      omsdk2.lib HINTS ${OPENSTAGESDK_DIR}/lib)
find_library(OPENSTAGESDK_UUID_LIBRARY omuuid.lib HINTS ${OPENSTAGESDK_DIR}/lib)

set(OPENSTAGESDK_INCLUDE_DIRS ${OPENSTAGESDK_INCLUDE_DIR})
set(OPENSTAGESDK_LIBRARIES ${OPENSTAGESDK_LIBRARY} ${OPENSTAGESDK_UUID_LIBRARY})

set(OPENSTAGESDK_BINARIES
  ${OPENSTAGESDK_DIR}/bin/omsdk2.dll
  ${OPENSTAGESDK_DIR}/bin/libglog.dll
  ${OPENSTAGESDK_DIR}/bin/tbb.dll
)
