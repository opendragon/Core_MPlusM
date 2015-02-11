//--------------------------------------------------------------------------------------------------
//
//  File:       M+MBridgeClientMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the client of the address service.
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

#include "M+MBridgeClient.h"
#include "M+MBridgeRequests.h"

#include <mpm/M+MRequests.h>
#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file 
 @brief The main application for the client of the bridge service. */

/*! @dir BridgeClient
 @brief The set of files that implement the bridge client. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Bridge;
using namespace MplusM::Common;
using std::cerr;
using std::cin;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Process the argument list for the application.
 @param arguments The arguments to analyze.
 @param namePattern The generated search value. */
static void processArguments(const StringVector &    arguments,
                             yarp::os::ConstString & namePattern)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("arguments = ", &arguments, "namePattern = ", &namePattern); //####
    yarp::os::ConstString tag;
    
    for (int ii = 0, argc = arguments.size(); argc > ii; ++ii)
    {
        tag = arguments[ii];
    }
    if (0 < tag.length())
    {
        namePattern = "'" + namePattern + " " + tag + "'";
    }
    OD_LOG_EXIT(); //####
} // processArguments

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

#if 0
1) The new M+M Unreal service, Unreal2, starts up.
2) Unreal2 creates a 'listen' port.
3) Unreal2 connects to Vicon2.
4) Unreal2 fetches the 'listen' port from Vicon2.
5) Unreal2 opens a connection to the Vicon2 'listen' port.
6) Unreal2 waits for a connection from Unreal.
6a) If a connection from Unreal, set up an outgoing port.
7) Repeat step 6 until there's a connection from Unreal.
8) Unreal2 waits for data from Vicon2.
9) Unreal2 sends the data through the outgoing port.
10) Repeat steps 8 and 9 indefinitely.
#endif//0

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief The entry point for communicating with the Address service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the address client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
#if MAC_OR_LINUX_
# pragma unused(argc)
#endif // MAC_OR_LINUX_
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(*argv);
#endif // MAC_OR_LINUX_
    try
    {
        Utilities::SetUpGlobalStatusReporter();
#if defined(MpM_ReportOnConnections)
        ChannelStatusReporter * reporter = Utilities::GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)
        OutputFlavour           flavour; // ignored
        StringVector            arguments;
        
        if (Utilities::ProcessStandardUtilitiesOptions(argc, argv,
                                                       " [tag]\n\n"
                                                       "  tag        The tag for the service to be "
                                                       "connnected to", flavour, &arguments))
        {
            try
            {
                Utilities::CheckForNameServerReporter();
    #if CheckNetworkWorks_
                if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))
    #endif // CheckNetworkWorks_
                {
                    yarp::os::Network     yarp; // This is necessary to establish any connections to
                                                // the YARP infrastructure
                    yarp::os::ConstString channelNameRequest(MpM_REQREP_DICT_NAME_KEY ":");
                    yarp::os::ConstString namePattern(MpM_BRIDGE_CANONICAL_NAME);
                    
                    Initialize(*argv);
                    BridgeClient * stuff = new BridgeClient;
                    
                    if (stuff)
                    {
#if defined(MpM_ReportOnConnections)
                        stuff->setReporter(reporter, true);
#endif // defined(MpM_ReportOnConnections)
                        if (0 < arguments.size())
                        {
                            yarp::os::ConstString tag(arguments[0]);
                            
                            if (0 < tag.length())
                            {
                                namePattern = "'" + namePattern + " " + tag + "'";
                            }
                        }
                        channelNameRequest += namePattern;
                        if (stuff->findService(channelNameRequest.c_str()))
                        {
                            if (stuff->connectToService())
                            {
                                yarp::os::ConstString address;
                                int                   port;
                                
                                if (stuff->getAddress(address, port))
                                {

                                    
                                    //TBD!!!!
                                    
                                    
                                    
                                    
                                }
                                else
                                {
                                    OD_LOG("! (stuff->getAddress(address, port))"); //####
#if MAC_OR_LINUX_
                                    GetLogger().fail("Problem fetching the address information.");
#endif // MAC_OR_LINUX_
                                }
                            }
                            else
                            {
                                OD_LOG("! (stuff->connectToService())"); //####
#if MAC_OR_LINUX_
                                GetLogger().fail("Could not connect to the required service.");
#else // ! MAC_OR_LINUX_
                                cerr << "Could not connect to the required service." << endl;
#endif // ! MAC_OR_LINUX_
                            }
                        }
                        else
                        {
                            OD_LOG("! (stuff->findService(channelNameRequest)"); //####
#if MAC_OR_LINUX_
                            GetLogger().fail("Could not find the required service.");
#else // ! MAC_OR_LINUX_
                            cerr << "Could not find the required service." << endl;
#endif // ! MAC_OR_LINUX_
                        }
                        delete stuff;
                    }
                    else
                    {
                        OD_LOG("! (stuff)"); //####
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
        Utilities::ShutDownGlobalStatusReporter();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_
