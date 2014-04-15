//--------------------------------------------------------------------------------------
//
//  File:       ServiceLister/main.cpp
//
//  Project:    M+M
//
//  Contains:   A utility application to list the available services.
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
//  Created:    2014-03-12
//
//--------------------------------------------------------------------------------------

#include "M+MBaseClient.h"
#include "M+MClientChannel.h"
#include "M+MRequests.h"
#include "M+MServiceRequest.h"
#include "M+MServiceResponse.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#include <iostream>
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
#include <yarp/os/all.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief A utility application to list the available services. */

/*! @dir ServiceLister
 @brief The ServiceLister application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using std::cout;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Retrieve the details for a service.
 @param serviceChannelName The channel for the service.
 @param canonicalName The canonical name for the service.
 @param description The description of the service.
 @returns @c true if the service returned the desired information and @c false otherwise. */
static bool getNameAndDescriptionForService(const yarp::os::ConstString & serviceChannelName,
                                            yarp::os::ConstString &       canonicalName,
                                            yarp::os::ConstString &       description)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("serviceChannelName = ", serviceChannelName.c_str());//####
    bool                            result = false;
    yarp::os::ConstString           aName(MplusM::Common::GetRandomChannelName("/servicelister/channel_"));
    MplusM::Common::ClientChannel * newChannel = new MplusM::Common::ClientChannel;
    
    if (newChannel)
    {
        if (newChannel->openWithRetries(aName))
        {
            if (MplusM::Common::NetworkConnectWithRetries(aName, serviceChannelName))
            {
                MplusM::Common::Package         parameters;
                MplusM::Common::ServiceRequest  request(MpM_NAME_REQUEST, parameters);
                MplusM::Common::ServiceResponse response;
                
                if (request.send(*newChannel, &response))
                {
                    OD_LOG_S1("response <- ", response.asString().c_str());//####
                    if (MpM_EXPECTED_NAME_RESPONSE_SIZE == response.count())
                    {
                        yarp::os::Value theCanonicalName(response.element(0));
                        yarp::os::Value theDescription(response.element(1));
                        
                        OD_LOG_S2("theCanonicalName <- ", theCanonicalName.toString().c_str(),//####
                                  "theDescription <- ", theDescription.toString().c_str());//####
                        if (theCanonicalName.isString() && theDescription.isString())
                        {
                            canonicalName = theCanonicalName.toString();
                            description = theDescription.toString();
                            result = true;
                        }
                        else
                        {
                            OD_LOG("! (theCanonicalName.isString() && theDescription.isString())");//####
                        }
                    }
                    else
                    {
                        OD_LOG("! (MpM_EXPECTED_NAME_RESPONSE_SIZE == response.count())");//####
                        OD_LOG_S1("response = ", response.asString().c_str());//####
                    }
                }
                else
                {
                    OD_LOG("! (request.send(*newChannel, &response))");//####
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
} // getNameAndDescriptionForService

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for listing the available services.

 There are no input arguments and standard output will receive a list of the available services.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
#pragma unused(argc)
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID |//####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr);//####
    OD_LOG_ENTER();//####
    try
    {
        if (yarp::os::Network::checkNetwork())
        {
            yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
            
            MplusM::Common::Initialize(*argv);
            MplusM::Common::Package matches(MplusM::Common::FindMatchingServices(MpM_REQREP_DICT_REQUEST_KEY ":*"));
            
            if (MpM_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())
            {
                // First, check if the search succeeded.
                yarp::os::ConstString matchesFirstString(matches.get(0).toString());
                
                if (strcmp(MpM_OK_RESPONSE, matchesFirstString.c_str()))
                {
                    OD_LOG("(strcmp(MpM_OK_RESPONSE, matchesFirstString.c_str()))");//####
                    yarp::os::ConstString reason(matches.get(1).toString());
                    
                    cerr << "Failed: " << reason.c_str() << "." << endl;
                }
                else
                {
                    // Now, process the second element.
                    MplusM::Common::Package * matchesList = matches.get(1).asList();
                    
                    if (matchesList)
                    {
                        bool reported = false;
                        int  matchesCount = matchesList->size();
                        
                        if (matchesCount)
                        {
                            for (int ii = 0; ii < matchesCount; ++ii)
                            {
                                yarp::os::ConstString aMatch(matchesList->get(ii).toString());
                                yarp::os::ConstString canonicalName;
                                yarp::os::ConstString description;
                                
                                if (getNameAndDescriptionForService(aMatch, canonicalName, description))
                                {
                                    if (! reported)
                                    {
                                        cout << "Services: " << endl;
                                    }
                                    reported = true;
                                    cout << endl;
                                    cout << "Service port: " << aMatch.c_str() << endl;
                                    cout << "Service name: " << canonicalName.c_str() << endl;
                                    cout << "Description:  " << description.c_str() << endl;
                                }
                            }
                            cout << endl;
                        }
                        if (! reported)
                        {
                            cout << "No services found." << endl;
                        }
                    }
                    else
                    {
                        OD_LOG("! (matchesList)");//####
                    }
                }
            }
            else
            {
                OD_LOG("! (MpM_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())");//####
                cerr << "Problem getting information from the Service Registry." << endl;
            }
        }
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork())");//####
            cerr << "YARP network not running." << endl;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0);//####
    return 0;
} // main
