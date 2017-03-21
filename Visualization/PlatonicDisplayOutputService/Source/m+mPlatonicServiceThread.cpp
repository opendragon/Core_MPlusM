//--------------------------------------------------------------------------------------------------
//
//  File:       m+mPlatonicServiceThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for an output service thread for the platonic display output
//              service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2016 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2016-06-04
//
//--------------------------------------------------------------------------------------------------

#include "m+mPlatonicServiceThread.hpp"
#include "m+mPlatonicDisplayOutputService.hpp"

//#include <ODEnableLogging.h>
#include <ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for an output service thread for the platonic display output
 service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace PlatonicDisplay;

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

PlatonicServiceThread::PlatonicServiceThread(PlatonicDisplayOutputService * service,
                                     const YarpString &         helpText) :
    inherited("data collector"), _service(service), _helpText(helpText)
{
    ODL_ENTER(); //####
    ODL_P1("service = ", service); //####
    ODL_S1s("helpText = ", helpText); //####
    ODL_EXIT_P(this); //####
} // PlatonicServiceThread::PlatonicServiceThread

PlatonicServiceThread::~PlatonicServiceThread(void)
{
    ODL_OBJENTER(); //####
    setService(NULL);
    ODL_OBJEXIT(); //####
} // PlatonicServiceThread::~PlatonicServiceThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
PlatonicServiceThread::run(void)
{
    ODL_OBJENTER(); //####
    if (_service)
    {
        ODL_LOG("(_service)"); //####
        // need a way to gonnect 'threadShouldExit' to the service
        _service->performLaunch(_helpText, true, false, false);
        // If we get here, the service has stopped running!
        setService(NULL);
    }
    ODL_OBJEXIT(); //####
} // PlatonicServiceThread::run

void
PlatonicServiceThread::setService(PlatonicDisplayOutputService * aService)
{
    ODL_OBJENTER(); //####
    ODL_P1("aService = ", aService); //####
    _service = aService;
    ODL_P1("_service <- ", _service); //####
    ODL_OBJEXIT(); //####
} // PlatonicServiceThread::setService

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
