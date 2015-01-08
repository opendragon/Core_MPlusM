//--------------------------------------------------------------------------------------------------
//
//  File:       M+MJavaScriptService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the JavaScript input / output service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by HPlus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2015-01-05
//
//--------------------------------------------------------------------------------------------------

#include "M+MJavaScriptService.h"
#include "M+MJavaScriptInputHandler.h"
#include "M+MJavaScriptRequests.h"

#include <mpm/M+MEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Winvalid-offsetof"
#endif // defined(__APPLE__)
#include <js/RequiredDefines.h>
#include <jsapi.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the %JavaScript input / output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::JavaScript;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
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

JavaScriptService::JavaScriptService(JSContext *                   context,
                                     const yarp::os::ConstString & launchPath,
                                     const yarp::os::ConstString & tag,
                                     const yarp::os::ConstString & description,
                                     const Common::ChannelVector & loadedInletDescriptions,
                                     const Common::ChannelVector & loadedOutletDescriptions,
                                     const yarp::os::ConstString & serviceEndpointName,
                                     const yarp::os::ConstString & servicePortNumber) :
    inherited(launchPath, tag, true, MpM_JAVASCRIPT_CANONICAL_NAME, description, "",
              serviceEndpointName, servicePortNumber),
    _context(context), _loadedInletDescriptions(loadedInletDescriptions),
    _loadedOutletDescriptions(loadedOutletDescriptions), _inHandler(new JavaScriptInputHandler)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("context = ", context, "loadedInletDescriptions = ", &loadedInletDescriptions, //####
              "loadedOutletDescriptions = ", &loadedOutletDescriptions); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "description = ", description, //####
               "serviceEndpointName = ", serviceEndpointName); //####
    OD_LOG_S1s("servicePortNumber = ", servicePortNumber); //####
    JS_SetContextPrivate(context, this);
    OD_LOG_EXIT_P(this); //####
} // JavaScriptService::JavaScriptService

JavaScriptService::~JavaScriptService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    delete _inHandler;
    OD_LOG_OBJEXIT(); //####
} // JavaScriptService::~JavaScriptService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool JavaScriptService::configure(const yarp::os::Bottle & details)
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(details)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = false;
    
    try
    {
        // Nothing needs to be done.
        result = true;
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(); //####
    return result;
} // JavaScriptService::configure
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void JavaScriptService::restartStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        // No special processing needed.
        stopStreams();
        startStreams();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // JavaScriptService::restartStreams

bool JavaScriptService::setUpStreamDescriptions(void)
{
    OD_LOG_OBJENTER(); //####
    bool                  result = true;
    ChannelDescription    description;
    yarp::os::ConstString rootName(getEndpoint().getName() + "/");
    
    _inDescriptions.clear();
    for (ChannelVector::const_iterator walker(_loadedInletDescriptions.begin());
         _loadedInletDescriptions.end() != walker; ++walker)
    {
        description._portName = rootName + walker->_portName;
        description._portProtocol = walker->_portProtocol;
        description._protocolDescription = walker->_protocolDescription;
        _inDescriptions.push_back(description);
    }
    _outDescriptions.clear();
    for (ChannelVector::const_iterator walker(_loadedOutletDescriptions.begin());
         _loadedOutletDescriptions.end() != walker; ++walker)
    {
        description._portName = rootName + walker->_portName;
        description._portProtocol = walker->_portProtocol;
        description._protocolDescription = walker->_protocolDescription;
        _outDescriptions.push_back(description);
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // JavaScriptService::setUpStreamDescriptions

bool JavaScriptService::start(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isStarted())
        {
            inherited::start();
            if (isStarted())
            {
                
            }
            else
            {
                OD_LOG("! (isStarted())"); //####
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(isStarted()); //####
    return isStarted();
} // JavaScriptService::start

void JavaScriptService::startStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_inHandler)
            {
//                _inHandler->setOutput(_outStreams.at(0));
                for (int ii = 0, mm = _inStreams.size(); mm > ii; ++ii)
                {
                    _inStreams.at(ii)->setReader(*_inHandler);
                }
                _inHandler->activate();
                setActive();
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // JavaScriptService::startStreams

bool JavaScriptService::stop(void)
{
    OD_LOG_OBJENTER(); //####
    bool result;
    
    try
    {
        result = inherited::stop();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // JavaScriptService::stop

void JavaScriptService::stopStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            if (_inHandler)
            {
                _inHandler->deactivate();
//                _inHandler->setOutput(NULL);
            }
            clearActive();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // JavaScriptService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
