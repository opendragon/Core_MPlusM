//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseAdapterService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the minimal functionality required for an m+m
//              adapter service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2015-05-27
//
//--------------------------------------------------------------------------------------------------

#include "m+mBaseAdapterService.h"

#include <m+m/m+mBaseAdapterData.h>
#include <m+m/m+mBaseClient.h>
#include <m+m/m+mRequests.h>
#include <m+m/m+mUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the minimal functionality required for an m+m adapter service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cerr;
using std::endl;

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

BaseAdapterService::BaseAdapterService(const Utilities::DescriptorVector & argumentList,
                                       const YarpString &                  launchPath,
                                       const int                           argc,
                                       char * *                            argv,
                                       const YarpString &                  tag,
                                       const bool                          useMultipleHandlers,
                                       const YarpString &                  canonicalName,
                                       const YarpString &                  description,
                                       const YarpString &                  requestsDescription,
                                       const YarpString &                  serviceEndpointName,
                                       const YarpString &                  servicePortNumber) :
    inherited(argumentList, kServiceKindAdapter, launchPath, argc, argv, tag, useMultipleHandlers,
              canonicalName, description, requestsDescription, serviceEndpointName,
              servicePortNumber)
{
    ODL_ENTER(); //####
    ODL_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    ODL_S4s("launchPath = ", launchPath, "tag = ", tag, "canonicalName = ", canonicalName, //####
               "description = ", description); //####
    ODL_S3s("requestsDescription = ", requestsDescription, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    ODL_LL1("argc = ", argc); //####
    ODL_B1("useMultipleHandlers = ", useMultipleHandlers); //####
    ODL_EXIT_P(this); //####
} // BaseAdapterService::BaseAdapterService

BaseAdapterService::~BaseAdapterService(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // BaseAdapterService::~BaseAdapterService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
BaseAdapterService::performLaunch(BaseAdapterData &  sharedData,
                                  const YarpString & helpText,
                                  const bool         goWasSet,
                                  const bool         stdinAvailable,
                                  const bool         reportOnExit)
{
    ODL_P1("sharedData = ", &sharedData); //####
    ODL_S1s("helpText = ", helpText); //####
    ODL_B3("goWasSet = ", goWasSet, "stdinAvailable = ", stdinAvailable, //####
              "reportOnExit = ", reportOnExit); //####
    BaseClient * aClient = sharedData.getClient();
    
    startPinger();
    sharedData.activate();
    runService("", true, goWasSet, stdinAvailable, reportOnExit);
    sharedData.deactivate();
    if (! aClient->disconnectFromService())
    {
        ODL_LOG("(! aClient->disconnectFromService())"); //####
        MpM_FAIL_(MSG_COULD_NOT_DISCONNECT_FROM_SERVICE);
    }
} // BaseAdapterService::performLaunch

DEFINE_SETUPCLIENTSTREAMS_(BaseAdapterService)
{
    ODL_OBJENTER(); //####
    bool result = inherited::setUpClientStreams();
    
    if (result)
    {
        result = addClientStreamsFromDescriptions(_clientDescriptions);
    }
    ODL_EXIT_B(result); //####
    return result;
} // BaseAdapterService::setUpClientStreams

DEFINE_SETUPINPUTSTREAMS_(BaseAdapterService)
{
    ODL_OBJENTER(); //####
    bool result = inherited::setUpInputStreams();
    
    if (result)
    {
        result = addInStreamsFromDescriptions(_inDescriptions);
    }
    ODL_EXIT_B(result); //####
    return result;
} // BaseAdapterService::setUpInputStreams

DEFINE_SETUPOUTPUTSTREAMS_(BaseAdapterService)
{
    ODL_OBJENTER(); //####
    bool result = inherited::setUpOutputStreams();
    
    if (result)
    {
        result = addOutStreamsFromDescriptions(_outDescriptions);
    }
    ODL_EXIT_B(result); //####
    return result;
} // BaseAdapterService::setUpOutputStreams

DEFINE_SHUTDOWNCLIENTSTREAMS_(BaseAdapterService)
{
    ODL_OBJENTER(); //####
    bool result = inherited::shutDownClientStreams();
    
    ODL_EXIT_B(result); //####
    return result;
} // BaseAdapterService::shutDownClientStreams

DEFINE_SHUTDOWNINPUTSTREAMS_(BaseAdapterService)
{
    ODL_OBJENTER(); //####
    bool result = inherited::shutDownInputStreams();
    
    ODL_EXIT_B(result); //####
    return result;
} // BaseAdapterService::shutDownInputStreams

DEFINE_SHUTDOWNOUTPUTSTREAMS_(BaseAdapterService)
{
    ODL_OBJENTER(); //####
    bool result = inherited::shutDownOutputStreams();
    
    ODL_EXIT_B(result); //####
    return result;
} // BaseAdapterService::shutDownOutputStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
