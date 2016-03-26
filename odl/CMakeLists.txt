#--------------------------------------------------------------------------------------------------
#
#  File:       odl/CMakeLists.txt
#
#  Project:    odl
#
#  Contains:   The CMAKE definitions for the debugging library.
#
#  Written by: Norman Jaffe
#
#  Copyright:  (c) 2014 by OpenDragon.
#
#              All rights reserved. Redistribution and use in source and binary forms, with or
#              without modification, are permitted provided that the following conditions are met:
#                * Redistributions of source code must retain the above copyright notice, this list
#                  of conditions and the following disclaimer.
#                * Redistributions in binary form must reproduce the above copyright notice, this
#                  list of conditions and the following disclaimer in the documentation and / or
#                  other materials provided with the distribution.
#                * Neither the name of the copyright holders nor the names of its contributors may
#                  be used to endorse or promote products derived from this software without
#                  specific prior written permission.
#
#              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
#              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
#              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
#              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
#              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
#              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
#              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
#              DAMAGE.
#
#  Created:    2014-02-06
#
#--------------------------------------------------------------------------------------------------

set(THIS_TARGET odLogging)
set(OTHER_TARGET_CSHARP odLogging_swig_csharp)
set(OTHER_TARGET_PYTHON odLogging_swig_python)

if(APPLE)
    option(ODL_UseObjectiveCDebug "Use Objective C Debug" OFF)
    set(DO_SHARED "SHARED")
else(APPLE)
    set(ODL_UseObjectiveCDebug OFF)
    set(DO_SHARED "")
endif(APPLE)
mark_as_advanced(ODL_UseObjectiveCDebug)

option(ODL_DO_SWIG OFF)
mark_as_advanced(ODL_DO_SWIG)

if(ODL_UseObjectiveCDebug)
    add_library(${THIS_TARGET}
                ${DO_SHARED}
                ODLogging.m)
else(ODL_UseObjectiveCDebug)
    add_library(${THIS_TARGET}
                ${DO_SHARED}
                ODLogging.cpp)
endif(ODL_UseObjectiveCDebug)

install(TARGETS ${THIS_TARGET}
        DESTINATION lib
        COMPONENT libraries)

install(FILES
        ODDisableLogging.h
        ODEnableLogging.h
        ODLogging.h
        DESTINATION include/odl
        COMPONENT headers)

if(ODL_DO_SWIG)
    set(ODL_SWIG_SOURCES swig_odl_in/ODLogging.i ODLogging.h ODLogging.cpp)

#    include_directories("${PROJECT_SOURCE_DIR}/odl")
    set_source_files_properties(swig_odl_in/ODLogging.i PROPERTIES CPLUSPLUS ON)
    set_source_files_properties(swig_odl_in/ODLogging.i PROPERTIES SWIG_FLAGS "-includeall")

    set(CMAKE_SWIG_OUTDIR "${PROJECT_SOURCE_DIR}/odl/swig_odl_out_csharp")

    swig_add_module(${OTHER_TARGET_CSHARP} csharp ${ODL_SWIG_SOURCES})

    install(TARGETS ${OTHER_TARGET_CSHARP}
            DESTINATION lib
            COMPONENT libraries)

    install(FILES
            swig_odl_out_csharp/ODLogging.cs
            swig_odl_out_csharp/ODLoggingPINVOKE.cs
            DESTINATION csharp/odl
            COMPONENT headers)

    set(CMAKE_SWIG_OUTDIR "${PROJECT_SOURCE_DIR}/odl/swig_odl_out_python")

    find_package(PythonLibs)
    include_directories(${PYTHON_INCLUDE_PATH})

    swig_add_module(${OTHER_TARGET_PYTHON} python ${ODL_SWIG_SOURCES})

    swig_link_libraries(${OTHER_TARGET_PYTHON}
                        ${THIS_TARGET}
                        ${PYTHON_LIBRARIES})

# Note the modified target name!
    install(TARGETS _${OTHER_TARGET_PYTHON}
            DESTINATION lib
            COMPONENT libraries)

    install(FILES
            swig_odl_out_python/ODLogging.py
            swig_odl_out_python/ODLoggingPYTHON_wrap.cxx
            DESTINATION python/odl
            COMPONENT headers)

endif(ODL_DO_SWIG)
