//--------------------------------------------------------------------------------------------------
//
//  File:       EchoClientMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the client of the echo service.
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
//                  list of conditions and the following disclaimer in the documentation and/or
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#include "M+MChannelStatusReporter.h"
#include "M+MEchoClient.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The main application for the client of the echo service. */

/*! @dir EchoClient
 @brief The set of files that implement the client for the echo service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;
using std::cin;
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

/*! @brief The entry point for communicating with the echo service.
 
 Strings read from standard input will be sent to the service. Entering an end-of-file will exit the program.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
#if MAC_OR_LINUX_
# pragma unused(argc)
#endif // MAC_OR_LINUX_
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
    MplusM::Common::SetUpLogger(*argv);
    try
    {
        if (MplusM::CanReadFromStandardInput())
        {
#if CheckNetworkWorks_
            if (yarp::os::Network::checkNetwork())
#endif // CheckNetworkWorks_
            {
                yarp::os::Network yarp; // This is necessary to establish any connection to the
                                        // YARP infrastructure
                
                MplusM::Common::Initialize(*argv);
                EchoClient * stuff = new EchoClient;
                
                if (stuff)
                {
                    MplusM::StartRunning();
                    MplusM::Common::SetSignalHandlers(MplusM::SignalRunningStop);
                    if (stuff->findService("details Echo*"))
                    {
#if defined(MpM_ReportOnConnections)
                        stuff->setReporter(ChannelStatusReporter::gReporter, true);
#endif // defined(MpM_ReportOnConnections)
                        if (stuff->connectToService())
                        {
                            for ( ; MplusM::IsRunning(); )
                            {
                                yarp::os::ConstString incoming;
                                std::string           inputLine;
                                
                                cout << "Type something to be echoed: ";
                                if (getline(cin, inputLine))
                                {
                                    yarp::os::ConstString outgoing(inputLine.c_str());
                                    if (stuff->sendAndReceive(outgoing, incoming))
                                    {
                                        cout << "Received: '" << incoming.c_str() << "'." << endl;
                                    }
                                    else
                                    {
                                        OD_LOG("! (stuff->sendAndReceive(outgoing, " //####
                                               "incoming))"); //####
                                        MplusM::Common::GetLogger().fail("Problem communicating "
                                                                         "with the service.");
                                    }
                                }
                                else
                                {
                                    break;
                                }
                                
                            }
                            if (! stuff->disconnectFromService())
                            {
                                OD_LOG("(! stuff->disconnectFromService())"); //####
                                MplusM::Common::GetLogger().fail("Problem disconnecting from the "
                                                                 "service.");
                            }
                        }
                        else
                        {
                            OD_LOG("! (stuff->connectToService())"); //####
                            MplusM::Common::GetLogger().fail("Problem connecting to the service.");
                        }
                    }
                    else
                    {
                        OD_LOG("! (stuff->findService(\"details Echo*\"))"); //####
                        MplusM::Common::GetLogger().fail("Problem finding the service.");
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
                OD_LOG("! (yarp::os::Network::checkNetwork())"); //####
                MplusM::Common::GetLogger().fail("YARP network not running.");
            }
#endif // CheckNetworkWorks_
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
