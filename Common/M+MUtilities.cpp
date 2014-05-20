//--------------------------------------------------------------------------------------
//
//  File:       M+MUtilities.cpp
//
//  Project:    M+M
//
//  Contains:   The function and variable declarations for utilities for M+M clients and
//              services.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-03-19
//
//--------------------------------------------------------------------------------------

#include "M+MUtilities.h"
#include "M+MClientChannel.h"
#include "M+MRequests.h"
#include "M+MServiceRequest.h"
#include "M+MServiceResponse.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#include <yarp/os/Network.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The function and variable definitions for utilities for M+M clients and services. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Utilities;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The indicator string for the beginning of new information received. */
static const char * kLineMarker = "registration name ";

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Process the response from the name server.
 
 Note that each line of the response, except the last, is started with 'registration name'. This is followed by the
 port name, 'ip', the IP address, 'port' and the port number.
 @param received The response to be processed.
 @param ports The list of non-default ports/ipaddress/portnumber found. */
static void processNameServerResponse(const yarp::os::ConstString & received,
                                      PortVector &                  ports)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("received = ", received.c_str());//####
    size_t                lineMakerLength = strlen(kLineMarker);
    yarp::os::ConstString nameServerName(yarp::os::Network::getNameServerName());
    yarp::os::ConstString workingCopy(received);
    
    OD_LOG_S1("nameServerName = ", nameServerName.c_str());//####
    for (size_t nextPos = 0; yarp::os::ConstString::npos != nextPos; )
    {
        nextPos = workingCopy.find(kLineMarker);
        if (yarp::os::ConstString::npos != nextPos)
        {
            workingCopy = workingCopy.substr(nextPos + lineMakerLength);
            size_t chopPos = workingCopy.find(kLineMarker);
            
            if (yarp::os::ConstString::npos != chopPos)
            {
                char *                channelName;
                yarp::os::ConstString chopped(workingCopy.substr(0, chopPos));
                char *                choppedAsChars = strdup(chopped.c_str());
                char *                ipAddress;
                char *                saved;
                char *                pp = strtok_r(choppedAsChars, " ", &saved);
                
                if (pp)
                {
                    // Port name
                    if ('/' == *pp)
                    {
                        channelName = pp;
                        if (nameServerName == channelName)
                        {
                            pp = NULL;
                        }
                        else
                        {
                            pp = strtok_r(NULL, " ", &saved);
                        }
                    }
                    else
                    {
                        pp = NULL;
                    }
                }
                if (pp)
                {
                    // 'ip'
                    if (strcmp(pp, "ip"))
                    {
                        pp = NULL;
                    }
                    else
                    {
                        pp = strtok_r(NULL, " ", &saved);
                    }
                }
                if (pp)
                {
                    ipAddress = pp;
                    pp = strtok_r(NULL, " ", &saved);
                }
                if (pp)
                {
                    // 'port'
                    if (strcmp(pp, "port"))
                    {
                        pp = NULL;
                    }
                    else
                    {
                        pp = strtok_r(NULL, " ", &saved);
                    }
                }
                if (pp)
                {
                    PortDescriptor aDescriptor;
                    
                    aDescriptor._portName = channelName;
                    aDescriptor._portIpAddress = ipAddress;
                    aDescriptor._portPortNumber = pp;
                    ports.push_back(aDescriptor);
                }
                free(choppedAsChars);
            }
        }
    }
    OD_LOG_EXIT();//####
} // processNameServerResponse

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

bool MplusM::Utilities::CheckForRegistryService(const PortVector & ports)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("ports = ", &ports);//####
    bool result = false;
    
    for (size_t ii = 0, mm = ports.size(); mm > ii; ++ii)
    {
        const PortDescriptor & aDescriptor = ports[ii];
        
        if (aDescriptor._portName == MpM_REGISTRY_CHANNEL_NAME)
        {
            result = true;
            break;
        }
        
    }
    return result;
} // MplusM::Utilities::CheckForRegistryService

void MplusM::Utilities::GetDetectedPortList(PortVector & ports)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("ports = ", &ports);//####
    Package                request;
    Package                response;
    yarp::os::ContactStyle contactInfo;
    
    ports.clear();
    request.addString("list");
    contactInfo.timeout = 5.0;
    if (yarp::os::Network::writeToNameServer(request, response, contactInfo))
    {
        if (1 == response.size())
        {
            yarp::os::Value responseValue(response.get(0));
            
            if (responseValue.isString())
            {
                processNameServerResponse(responseValue.asString(), ports);
            }
            else
            {
                OD_LOG("! (responseValue.isString())");//####
            }
        }
        else
        {
            OD_LOG("! (1 == response.size())");//####
            OD_LOG_S1("response = ", response.toString().c_str());//####
        }
    }
    else
    {
        OD_LOG("! (yarp::os::Network::writeToNameServer(request, response))");//####
    }
    OD_LOG_EXIT();//####
} // MplusM::Utilities::GetDetectedPortList

bool MplusM::Utilities::GetNameAndDescriptionForService(const yarp::os::ConstString & serviceChannelName,
                                                        ServiceDescriptor &           descriptor)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("serviceChannelName = ", serviceChannelName.c_str());//####
    OD_LOG_P1("descriptor = ", &descriptor);//####
    bool                            result = false;
    yarp::os::ConstString           aName(MplusM::Common::GetRandomChannelName("/servicelister/channel_"));
    MplusM::Common::ClientChannel * newChannel = new MplusM::Common::ClientChannel;
    
    if (newChannel)
    {
        if (newChannel->openWithRetries(aName))
        {
            if (MplusM::Common::NetworkConnectWithRetries(aName, serviceChannelName))
            {
                MplusM::Common::Package         parameters1;
                MplusM::Common::ServiceRequest  request1(MpM_NAME_REQUEST, parameters1);
                MplusM::Common::ServiceResponse response1;
                
                if (request1.send(*newChannel, &response1))
                {
                    OD_LOG_S1("response1 <- ", response1.asString().c_str());//####
                    if (MpM_EXPECTED_NAME_RESPONSE_SIZE == response1.count())
                    {
                        yarp::os::Value theCanonicalName(response1.element(0));
                        yarp::os::Value theDescription(response1.element(1));
                        yarp::os::Value thePath(response1.element(2));
                        
                        OD_LOG_S3("theCanonicalName <- ", theCanonicalName.toString().c_str(),//####
                                  "theDescription <- ", theDescription.toString().c_str(), "thePath <- ",//####
                                  thePath.toString().c_str());//####
                        if (theCanonicalName.isString() && theDescription.isString() && thePath.isString())
                        {
                            descriptor._canonicalName = theCanonicalName.toString();
                            descriptor._description = theDescription.toString();
                            descriptor._path = thePath.toString();
                            result = true;
                        }
                        else
                        {
                            OD_LOG("! (theCanonicalName.isString() && theDescription.isString() && "//####
                                   "thePath.isString())");//####
                        }
                    }
                    else
                    {
                        OD_LOG("! (MpM_EXPECTED_NAME_RESPONSE_SIZE == response1.count())");//####
                        OD_LOG_S1("response1 = ", response1.asString().c_str());//####
                    }
                }
                else
                {
                    OD_LOG("! (request1.send(*newChannel, &response1))");//####
                }
                if (result)
                {
                    MplusM::Common::Package         parameters2;
                    MplusM::Common::ServiceRequest  request2(MpM_CHANNELS_REQUEST, parameters2);
                    MplusM::Common::ServiceResponse response2;
                    
                    if (request2.send(*newChannel, &response2))
                    {
                        OD_LOG_S1("response2 <- ", response2.asString().c_str());//####
                        for (int ii = 0, howMany = response2.count(); ii < howMany; ++ii)
                        {
                            yarp::os::Value element(response2.element(ii));
                            
                            if (element.isString())
                            {
                                descriptor._channels.push_back(element.toString());
                            }
                        }
                    }
                    else
                    {
                        OD_LOG("! (request2.send(*newChannel, &response2))");//####
                        result = false;
                    }
                }
#if defined(MpM_DO_EXPLICIT_DISCONNECT)
                if (! MplusM::Common::NetworkDisconnectWithRetries(aName, serviceChannelName))
                {
                    OD_LOG("(! MplusM::Common::NetworkDisconnectWithRetries(aName, destinationName))");//####
                }
#endif // defined(MpM_DO_EXPLICIT_DISCONNECT)
            }
            else
            {
                OD_LOG("! (MplusM::Common::NetworkConnectWithRetries(aName, serviceChannelName))");//####
            }
#if defined(MpM_DO_EXPLICIT_CLOSE)
            newChannel->close();
#endif // defined(MpM_DO_EXPLICIT_CLOSE)
        }
        else
        {
            OD_LOG("! (newChannel->openWithRetries(aName))");//####
        }
        delete newChannel;
    }
    else
    {
        OD_LOG("! (newChannel)");//####
    }
    OD_LOG_EXIT_B(result);//####
    return result;
} // MplusM::Utilities::GetNameAndDescriptionForService
