//--------------------------------------------------------------------------------------------------
//
//  File:       m+mAddressService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a service that returns an internet address upon request.
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

#include "m+mAddressService.hpp"
#include "m+mAddressRequests.hpp"
#include "m+mWhereRequestHandler.hpp"

//#include <ODEnableLogging.h>
#include <ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a service that returns an IP address and port upon request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Address;
using namespace MplusM::Common;

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

AddressService::AddressService(const YarpString & hostName,
                               const int          hostPort,
                               const YarpString & launchPath,
                               const int          argc,
                               char * *           argv,
                               const YarpString & tag,
                               const YarpString & serviceEndpointName,
                               const YarpString & servicePortNumber) :
    inherited(kServiceKindNormal, launchPath, argc, argv, tag, true, MpM_ADDRESS_CANONICAL_NAME_,
              ADDRESS_SERVICE_DESCRIPTION_,
              "where - return the matching internet address", serviceEndpointName,
              servicePortNumber), _address(hostName), _whereHandler(NULL), _port(hostPort)
{
    ODL_ENTER(); //####
    ODL_S4s("hostName = ", hostName, "launchPath = ", launchPath, "tag = ", tag, //####
            "serviceEndpointName = ", serviceEndpointName); //####
    ODL_S1s("servicePortNumber = ", servicePortNumber); //####
    ODL_LL2("hostPort = ", hostPort, "argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    std::stringstream buff;

    buff << "Host name is '" << _address.c_str() << "', host port is " << _port;
    setExtraInformation(buff.str());
    attachRequestHandlers();
    ODL_EXIT_P(this); //####
} // AddressService::AddressService

AddressService::~AddressService(void)
{
    ODL_OBJENTER(); //####
    detachRequestHandlers();
    ODL_OBJEXIT(); //####
} // AddressService::~AddressService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
AddressService::attachRequestHandlers(void)
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
} // AddressService::attachRequestHandlers

void
AddressService::detachRequestHandlers(void)
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
} // AddressService::detachRequestHandlers

void
AddressService::getAddress(YarpString & address,
                           int &        port)
{
    ODL_OBJENTER(); //####
    address = _address;
    port = _port;
    ODL_OBJEXIT(); //####
} // AddressService::getAddress

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
