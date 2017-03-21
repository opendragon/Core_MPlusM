//--------------------------------------------------------------------------------------------------
//
//  File:       m+mTunnelService.cpp
//
//  Project:    m+m
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

#include "m+mTunnelService.hpp"
#include "m+mConnectionThread.hpp"
#include "m+mTunnelRequests.hpp"
#include "m+mWhereRequestHandler.hpp"

#include <m+m/m+mEndpoint.hpp>

//#include <ODEnableLogging.h>
#include <ODLogging.h>

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
                             const int          argc,
                             char * *           argv,
                             const YarpString & tag,
                             const YarpString & serviceEndpointName,
                             const YarpString & servicePortNumber) :
    inherited(kServiceKindNormal, launchPath, argc, argv, tag, true, MpM_TUNNEL_CANONICAL_NAME_,
              TUNNEL_SERVICE_DESCRIPTION_,
              "where - return the matching internet address", serviceEndpointName,
              servicePortNumber), _listenAddress(""), _sourceAddress(sourceName),
    _whereHandler(NULL), _connection(new ConnectionThread(*this)), _listenPort(-1),
    _sourcePort(sourcePort)
{
    ODL_ENTER(); //####
    ODL_S4s("sourceName = ", sourceName, "launchPath = ", launchPath, "tag = ", tag, //####
            "serviceEndpointName = ", serviceEndpointName); //####
    ODL_S1s("servicePortNumber = ", servicePortNumber); //####
    ODL_L1("sourcePort = ", sourcePort); //####
    std::stringstream buff;

    buff << "Source name is '" << _sourceAddress.c_str() << "', source port is " << _sourcePort;
    setExtraInformation(buff.str());
    attachRequestHandlers();
    ODL_EXIT_P(this); //####
} // TunnelService::TunnelService

TunnelService::~TunnelService(void)
{
    ODL_OBJENTER(); //####
    detachRequestHandlers();
    if (_connection)
    {
        delete _connection;
        _connection = NULL;
    }
    ODL_OBJEXIT(); //####
} // TunnelService::~TunnelService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
TunnelService::attachRequestHandlers(void)
{
    ODL_OBJENTER(); //####
    try
    {
        _whereHandler = new WhereRequestHandler(*this);
        if (_whereHandler)
        {
            registerRequestHandler(_whereHandler);
        }
        else
        {
            ODL_LOG("! (_whereHandler)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // TunnelService::attachRequestHandlers

void
TunnelService::detachRequestHandlers(void)
{
    ODL_OBJENTER(); //####
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
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // TunnelService::detachRequestHandlers

void
TunnelService::getAddress(YarpString & address,
                          int &        port)
{
    ODL_OBJENTER(); //####
    address = _listenAddress;
    port = _listenPort;
    ODL_OBJEXIT(); //####
} // TunnelService::getAddress

bool
TunnelService::startService(void)
{
    ODL_OBJENTER(); //####
    bool result = false;

    try
    {
        if (! isStarted())
        {
            inherited::startService();
            if (isStarted())
            {
                yarp::os::Contact where = getEndpoint().where();

                _listenAddress = where.getHost();
                _listenPort = -1;
                if (_connection)
                {
                    if (_connection->isRunning())
                    {
                        _connection->stop();
                        for ( ; _connection->isRunning(); )
                        {
                            yarp::os::Time::delay(STANDARD_WAIT_TIME_ / 3.1);
                        }
                    }
                    _connection->setSourceAddress(_sourceAddress, _sourcePort);
                    if (! _connection->start())
                    {
                        ODL_LOG("(! _connection->start())"); //####
                        delete _connection;
                        _connection = NULL;
                    }
                }
            }
            else
            {
                ODL_LOG("! (isStarted())"); //####
            }
        }
        result = isStarted();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // TunnelService::startService

bool
TunnelService::stopService(void)
{
    ODL_OBJENTER(); //####
    bool result = false;

    try
    {
        if (_connection)
        {
            _connection->stop();
            for ( ; _connection->isRunning(); )
            {
                yarp::os::Time::delay(STANDARD_WAIT_TIME_ / 4.3);
            }
        }
        result = inherited::stopService();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // TunnelService::stopService

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
