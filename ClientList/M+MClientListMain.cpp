//--------------------------------------------------------------------------------------------------
//
//  File:       M+MClientListMain.cpp
//
//  Project:    M+M
//
//  Contains:   A utility application to list the clients of a service or all services.
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
//  Created:    2014-03-12
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MBaseClient.h>
#include <mpm/M+MClientChannel.h>
#include <mpm/M+MRequests.h>
#include <mpm/M+MServiceRequest.h>
#include <mpm/M+MServiceResponse.h>
#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief A utility application to list the clients of a service or all services. */

/*! @dir ClientList
 @brief The set of files that implement the Client List application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Process the response to the 'list' request sent to a service.
 @param flavour The format for the output.
 @param serviceName The name of the service that generated the response.
 @param response The response to be processed.
 @param sawResponse @c true if there was already a response output and @c false if this is the first.
 @returns @c true if some output was generated and @c false otherwise. */
static bool processResponse(OutputFlavour                 flavour,
                            const yarp::os::ConstString & serviceName,
                            const ServiceResponse &       response,
                            const bool                    sawResponse)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceName = ", serviceName); //####
    OD_LOG_P1("response = ", &response); //####
    bool                  result = false;
    yarp::os::ConstString cleanServiceName(SanitizeString(serviceName,
                                                          kOutputFlavourJSON != flavour));
    
    OD_LOG_S1s("response = ", response.asString()); //####
    for (int ii = 0, howMany = response.count(); ii < howMany; ++ii)
    {
        yarp::os::Value element(response.element(ii));
        
        if (element.isString())
        {
            yarp::os::ConstString clientString(element.toString());
            
            switch (flavour)
            {
                case kOutputFlavourJSON :
                    if (result || sawResponse)
                    {
                        cout << "," << endl;
                    }
                    cout << T_("{ " CHAR_DOUBLEQUOTE "Service" CHAR_DOUBLEQUOTE ": "
                               CHAR_DOUBLEQUOTE) << cleanServiceName.c_str() <<
                            T_(CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE "Client" CHAR_DOUBLEQUOTE ": "
                               CHAR_DOUBLEQUOTE) << SanitizeString(clientString, true).c_str() <<
                            T_(CHAR_DOUBLEQUOTE " }");
                    break;
                    
                case kOutputFlavourTabs :
                    cout << cleanServiceName.c_str() << "\t" <<
                            SanitizeString(clientString).c_str() << endl;
                    break;
                    
                case kOutputFlavourNormal :
                    if (! result)
                    {
                        cout << "Service: " << cleanServiceName.c_str() << endl << "Clients: " <<
                        endl;
                    }
                    cout << "   " << SanitizeString(clientString).c_str() << endl;
                    break;
                    
                default :
                    break;
                    
            }
            result = true;
        }
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // processResponse

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for listing the clients of a service.
 
 The optional argument is the name of the channel for the service. If the channel is not specified,
 all service channels will be reported. Standard output will receive a list of the specified
 clients.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(*argv);
#endif // MAC_OR_LINUX_
    OutputFlavour flavour;
    StringVector  arguments;
    
    if (Utilities::ProcessStandardUtilitiesOptions(argc, argv, " [channel]\n\n"
                                                   "  channel    Optional channel name for "
                                                   "service", flavour, &arguments))
    {
        try
        {
            Utilities::CheckForNameServerReporter();
#if CheckNetworkWorks_
            if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))
#endif // CheckNetworkWorks_
            {
                yarp::os::Network     yarp; // This is necessary to establish any connections to the
                                            // YARP infrastructure
                yarp::os::ConstString channelNameRequest(MpM_REQREP_DICT_CHANNELNAME_KEY ":");
                
                Initialize(*argv);
                if (0 < arguments.size())
                {
                    channelNameRequest += arguments[0];
                }
                else
                {
                    channelNameRequest += "*";
                }
                yarp::os::Bottle matches(FindMatchingServices(channelNameRequest));
                
                if (MpM_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())
                {
                    // First, check if the search succeeded.
                    yarp::os::ConstString matchesFirstString(matches.get(0).toString());
                    
                    if (strcmp(MpM_OK_RESPONSE, matchesFirstString.c_str()))
                    {
                        OD_LOG("(strcmp(MpM_OK_RESPONSE, matchesFirstString.c_str()))"); //####
#if MAC_OR_LINUX_
                        yarp::os::ConstString reason(matches.get(1).toString());
                        
                        GetLogger().fail(yarp::os::ConstString("Failed: ") + reason + ".");
#endif // MAC_OR_LINUX_
                    }
                    else
                    {
                        // Now, process the second element.
                        yarp::os::Bottle * matchesList = matches.get(1).asList();
                        
                        if (matchesList)
                        {
                            int matchesCount = matchesList->size();
                            
                            if (matchesCount)
                            {
                                yarp::os::ConstString aName =
                                                        GetRandomChannelName(HIDDEN_CHANNEL_PREFIX
                                                                             "clientlist_/"
                                                                             DEFAULT_CHANNEL_ROOT);
                                ClientChannel *       newChannel = new ClientChannel;
                                
                                if (newChannel)
                                {
                                    if (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
                                    {
                                        bool             sawRequestResponse = false;
                                        yarp::os::Bottle parameters;
                                        
                                        if (kOutputFlavourJSON == flavour)
                                        {
                                            cout << "[ ";
                                        }
                                        for (int ii = 0; ii < matchesCount; ++ii)
                                        {
                                            yarp::os::ConstString aMatch =
                                            matchesList->get(ii).toString();
                                            
                                            if (Utilities::NetworkConnectWithRetries(aName, aMatch,
                                                                             STANDARD_WAIT_TIME))
                                            {
                                                ServiceRequest  request(MpM_CLIENTS_REQUEST,
                                                                        parameters);
                                                ServiceResponse response;
                                                
                                                if (request.send(*newChannel, &response))
                                                {
                                                    OD_LOG("(request.send(*newChannel, " //####
                                                           "&response))"); //####
                                                    if (0 < response.count())
                                                    {
                                                        OD_LOG("(0 < response.count())"); //####
                                                        if (processResponse(flavour, aMatch,
                                                                            response,
                                                                            sawRequestResponse))
                                                        {
                                                            sawRequestResponse = true;
                                                        }
                                                    }
                                                }
                                                else
                                                {
                                                    OD_LOG("! (request.send(*newChannel, " //####
                                                           "&response))"); //####
#if MAC_OR_LINUX_
                                                    yarp::os::impl::Logger & theLogger =
                                                                                        GetLogger();
                                                    
                                                    theLogger.fail(yarp::os::ConstString("Problem "
                                                                                    "communicating "
                                                                                         "with ") +
                                                                   aMatch + ".");
#endif // MAC_OR_LINUX_
                                                }
#if defined(MpM_DoExplicitDisconnect)
                                                if (! Utilities::NetworkDisconnectWithRetries(aName,
                                                                                          aMatch,
                                                                              STANDARD_WAIT_TIME))
                                                {
                                                    OD_LOG("(! Utilities::Network" //####
                                                           "DisconnectWithRetries(aName, " //####
                                                           "aMatch, STANDARD_WAIT_TIME))"); //####
                                                }
#endif // defined(MpM_DoExplicitDisconnect)
                                            }
                                            else
                                            {
                                                OD_LOG("! (Utilities::NetworkConnectWith" //####
                                                       "Retries(aName, aMatch, " //####
                                                       "STANDARD_WAIT_TIME))"); //####
                                            }
                                        }
                                        if (kOutputFlavourJSON == flavour)
                                        {
                                            cout << " ]" << endl;
                                        }
                                        if (! sawRequestResponse)
                                        {
                                            switch (flavour)
                                            {
                                                case kOutputFlavourJSON :
                                                case kOutputFlavourTabs :
                                                    break;
                                                    
                                                case kOutputFlavourNormal :
                                                    cout << "No client connections found." << endl;
                                                    break;
                                                    
                                                default :
                                                    break;
                                                    
                                            }
                                        }
#if defined(MpM_DoExplicitClose)
                                        newChannel->close();
#endif // defined(MpM_DoExplicitClose)
                                    }
                                    else
                                    {
                                        OD_LOG("! (newChannel->openWithRetries(aName, " //####
                                               "STANDARD_WAIT_TIME))"); //####
                                    }
                                    delete newChannel;
                                }
                                else
                                {
                                    OD_LOG("! (newChannel)"); //####
                                }
                            }
                            else
                            {
                                switch (flavour)
                                {
                                    case kOutputFlavourJSON :
                                    case kOutputFlavourTabs :
                                        break;
                                        
                                    case kOutputFlavourNormal :
                                        cout << "No services found." << endl;
                                        break;
                                        
                                    default :
                                        break;
                                        
                                }
                            }
                        }
                        else
                        {
                            OD_LOG("! (matchesList)"); //####
                        }
                    }
                }
                else
                {
                    OD_LOG("! (MpM_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("Problem getting information from the Registry Service.");
#endif // MAC_OR_LINUX_
                }
            }
#if CheckNetworkWorks_
            else
            {
                OD_LOG("! (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))"); //####
# if MAC_OR_LINUX_
                GetLogger().fail("YARP network not running.");
# else // ! MAC_OR_LINUX_
                cerr << "YARP network not running." << endl;
# endif // ! MAC_OR_LINUX_
            }
#endif // CheckNetworkWorks_
        }
        catch (...)
        {
            OD_LOG("Exception caught"); //####
        }
        yarp::os::Network::fini();
    }
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
