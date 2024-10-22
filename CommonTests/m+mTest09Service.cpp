//--------------------------------------------------------------------------------------------------
//
//  File:       CommonTests/m+mTest09Service.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a simple service used by the unit tests.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and / or
//                  other materials provided with the distribution.
//                * Neither the name of the copyright holders nor the names of its contributors may
//                  be used to endorse or promote products derived from this software without
//                  specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
//              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
//              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
//              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
//              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//              DAMAGE.
//
//  Created:    2014-02-28
//
//--------------------------------------------------------------------------------------------------

#include "m+mTest09Service.hpp"
#include "m+mTest09DefaultRequestHandler.hpp"

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a simple service used by the unit tests. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Test;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

Test09Service::Test09Service(const YarpString & launchPath,
                             const int          argc,
                             char * *           argv) :
inherited(kServiceKindNormal, launchPath, argc, argv, false,
          "Test09", "Simple service for unit tests", ""), _defaultHandler(NULL)
{
    ODL_ENTER(); //####
    ODL_S1s("launchPath = ", launchPath); //####
    ODL_I1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    attachRequestHandlers();
    ODL_EXIT_P(this); //####
} // Test09Service::Test09Service

Test09Service::~Test09Service(void)
{
    ODL_OBJENTER(); //####
    detachRequestHandlers();
    ODL_OBJEXIT(); //####
} // Test09Service::~Test09Service

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
Test09Service::attachRequestHandlers(void)
{
    ODL_OBJENTER(); //####
    try
    {
        _defaultHandler = new Test09DefaultRequestHandler(*this);
        if (_defaultHandler)
        {
            setDefaultRequestHandler(_defaultHandler);
        }
        else
        {
            ODL_LOG("! (_defaultHandler)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // Test09Service::attachRequestHandlers

void
Test09Service::detachRequestHandlers(void)
{
    ODL_OBJENTER(); //####
    try
    {
        if (_defaultHandler)
        {
            setDefaultRequestHandler(NULL);
            delete _defaultHandler;
            _defaultHandler = NULL;
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // Test09Service::detachRequestHandlers

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
