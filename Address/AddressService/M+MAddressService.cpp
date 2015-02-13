//--------------------------------------------------------------------------------------------------
//
//  File:       M+MAddressService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a service that returns an internet address upon request.
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
//  Created:    2015-02-11
//
//--------------------------------------------------------------------------------------------------

#include "M+MAddressService.h"
#include "M+MAddressRequests.h"
#include "M+MWhereRequestHandler.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if (! MAC_OR_LINUX_)
# pragma comment(lib, "ws2_32.lib")
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a service that returns an IP address and port upon request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Address;
using namespace MplusM::Common;

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

AddressService::AddressService(const yarp::os::ConstString & hostName,
                               const int                     hostPort,
                               const yarp::os::ConstString & launchPath,
                               const yarp::os::ConstString & tag,
                               const yarp::os::ConstString & serviceEndpointName,
                               const yarp::os::ConstString & servicePortNumber) :
    inherited(kServiceKindNormal, launchPath, tag, true, MpM_ADDRESS_CANONICAL_NAME,
              "The Address service",
              "where - return the matching internet address", serviceEndpointName,
              servicePortNumber), _address(hostName), _whereHandler(NULL), _port(hostPort)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("hostName = ", hostName, "launchPath = ", launchPath, "tag = ", tag, //####
               "serviceEndpointName = ", serviceEndpointName); //####
    OD_LOG_S1s("servicePortNumber = ", servicePortNumber); //####
    OD_LOG_L1("hostPort = ", hostPort); //####
    attachRequestHandlers();
    OD_LOG_EXIT_P(this); //####
} // AddressService::AddressService

AddressService::~AddressService(void)
{
    OD_LOG_OBJENTER(); //####
    detachRequestHandlers();
    OD_LOG_OBJEXIT(); //####
} // AddressService::~AddressService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void AddressService::attachRequestHandlers(void)
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
} // AddressService::attachRequestHandlers

void AddressService::detachRequestHandlers(void)
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
} // AddressService::detachRequestHandlers

void AddressService::getAddress(yarp::os::ConstString & address,
                                int &                   port)
{
    OD_LOG_OBJENTER(); //####
    address = _address;
    port = _port;
    OD_LOG_OBJEXIT(); //####
} // AddressService::getAddress

bool AddressService::start(void)
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
} // AddressService::start

bool AddressService::stop(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = false;
    
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
} // AddressService::stop

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
