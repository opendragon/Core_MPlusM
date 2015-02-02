//--------------------------------------------------------------------------------------------------
//
//  File:       M+MServiceMetricsMain.cpp
//
//  Project:    M+M
//
//  Contains:   A utility application to display the metrics for one or more services.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-03-13
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

#if (! MAC_OR_LINUX_) //ASSUME WINDOWS
# include <mpm/getopt.h>
#endif //(! MAC_OR_LINUX_)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)

/*! @file
 @brief A utility application to display the metrics for one or more services. */

/*! @dir ServiceMetrics
 @brief The set of files that implement the service metrics application. */
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

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for displaying service metrics.
 
 The first, optional, argument is the name of the channel for the service. If the channel is not
 specified, all service channels will be reported. Standard output will receive a list of the
 specified metrics.
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
    
    if (Utilities::ProcessStandardUtilitiesOptions(argc, argv, " [channel]", flavour))
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
                if (optind >= argc)
                {
                    channelNameRequest += "*";
                }
                else
                {
                    channelNameRequest += argv[optind];
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
                                bool sawResponse = false;
                                
                                if (kOutputFlavourJSON == flavour)
                                {
                                    cout << "[ ";
                                }
                                for (int ii = 0; ii < matchesCount; ++ii)
                                {
                                    yarp::os::ConstString aMatch = matchesList->get(ii).toString();
                                    yarp::os::Bottle      metrics;
                                    
                                    if (Utilities::GetMetricsForService(aMatch, metrics,
                                                                        STANDARD_WAIT_TIME))
                                        
                                    {
                                        yarp::os::ConstString responseAsString =
                                                        Utilities::ConvertMetricsToString(metrics,
                                                                                          flavour);
                                        
                                        if (sawResponse)
                                        {
                                            switch (flavour)
                                            {
                                                case kOutputFlavourTabs :
                                                    cout << endl;
                                                    break;
                                                    
                                                case kOutputFlavourJSON :
                                                    cout << "," << endl;
                                                    break;
                                                    
                                                case kOutputFlavourNormal :
                                                    cout << endl << endl;
                                                    break;
                                                    
                                                default :
                                                    break;
                                                    
                                            }
                                        }
                                        sawResponse = true;
                                        if (kOutputFlavourNormal == flavour)
                                        {
                                            cout << SanitizeString(aMatch, true).c_str() << endl;
                                        }
                                        cout << responseAsString.c_str();
                                    }
                                }
                                if (kOutputFlavourJSON == flavour)
                                {
                                    cout << " ]";
                                }
                                if (sawResponse)
                                {
                                    cout << endl;
                                }
                                else
                                {
                                    switch (flavour)
                                    {
                                        case kOutputFlavourJSON :
                                        case kOutputFlavourTabs :
                                            break;
                                            
                                        case kOutputFlavourNormal :
                                            cout << "No matching service found." << endl;
                                            break;
                                            
                                        default :
                                            break;
                                            
                                    }
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
