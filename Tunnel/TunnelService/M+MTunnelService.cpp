//--------------------------------------------------------------------------------------------------
//
//  File:       M+MTunnelService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a service that routes non-YARP data over a YARP network.
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
//  Created:    2015-02-11
//
//--------------------------------------------------------------------------------------------------

#include "M+MTunnelService.h"
#include "M+MConnectionThread.h"
#include "M+MTunnelRequests.h"
#include "M+MWhereRequestHandler.h"

#include <mpm/M+MEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a service that routes non-YARP data over a YARP network. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Tunnel;

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

TunnelService::TunnelService(const YarpString & sourceName,
                             const int          sourcePort,
                             const YarpString & launchPath,
                             const YarpString & tag,
                             const YarpString & serviceEndpointName,
                             const YarpString & servicePortNumber) :
    inherited(kServiceKindNormal, launchPath, tag, true, MpM_TUNNEL_CANONICAL_NAME,
              TUNNEL_SERVICE_DESCRIPTION,
              "where - return the matching internet address", serviceEndpointName,
              servicePortNumber), _listenAddress(""), _sourceAddress(sourceName),
    _whereHandler(NULL), _connection(new ConnectionThread(*this)), _listenPort(-1),
    _sourcePort(sourcePort)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("sourceName = ", sourceName, "launchPath = ", launchPath, "tag = ", tag, //####
               "serviceEndpointName = ", serviceEndpointName); //####
    OD_LOG_S1s("servicePortNumber = ", servicePortNumber); //####
    OD_LOG_L1("sourcePort = ", sourcePort); //####
    attachRequestHandlers();
    OD_LOG_EXIT_P(this); //####
} // TunnelService::TunnelService

TunnelService::~TunnelService(void)
{
    OD_LOG_OBJENTER(); //####
    detachRequestHandlers();
    if (_connection)
    {
        delete _connection;
        _connection = NULL;
    }
    OD_LOG_OBJEXIT(); //####
} // TunnelService::~TunnelService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void TunnelService::attachRequestHandlers(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        _whereHandler = new WhereRequestHandler(*this);
        if (_whereHandler)
        {
            registerRequestHandler(_whereHandler);
        }
        else
        {
            OD_LOG("! (_whereHandler)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // TunnelService::attachRequestHandlers

void TunnelService::detachRequestHandlers(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (_whereHandler)
        {
            unregisterRequestHandler(_whereHandler);
            delete _whereHandler;
            _whereHandler = NULL;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // TunnelService::detachRequestHandlers

void TunnelService::getAddress(YarpString & address,
                               int &        port)
{
    OD_LOG_OBJENTER(); //####
    address = _listenAddress;
    port = _listenPort;
    OD_LOG_OBJEXIT(); //####
} // TunnelService::getAddress

bool TunnelService::start(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = false;
    
    try
    {
        if (! isStarted())
        {
            inherited::start();
            if (isStarted())
            {
                yarp::os::Contact where = getEndpoint().where();
                
                _listenAddress = where.getHost();
                _listenPort = -1;
                if (_connection->isRunning())
                {
                    _connection->stop();
                    for ( ; _connection->isRunning(); )
                    {
                        yarp::os::Time::delay(STANDARD_WAIT_TIME / 3.1);
                    }
                }
                _connection->setSourceAddress(_sourceAddress, _sourcePort);
                _connection->start();
            }
            else
            {
                OD_LOG("! (isStarted())"); //####
            }
        }
        result = isStarted();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // TunnelService::start

bool TunnelService::stop(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = false;
    
    try
    {
        if (_connection)
        {
            _connection->stop();
            for ( ; _connection->isRunning(); )
            {
                yarp::os::Time::delay(STANDARD_WAIT_TIME / 4.3);
            }
        }
        result = inherited::stop();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // TunnelService::stop

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
