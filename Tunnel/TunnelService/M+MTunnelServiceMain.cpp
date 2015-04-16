//--------------------------------------------------------------------------------------------------
//
//  File:       M+MTunnelServiceMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the Tunnel service.
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

#include <mpm/M+MEndpoint.h>
#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the Tunnel service. */

/*! @dir Tunnel
 @brief The set of files that support routing non-YARP data via YARP. */

/*! @dir TunnelCommon
 @brief The set of files that are shared between the Tunnel client and Tunnel service. */

/*! @dir TunnelService
 @brief The set of files that implement the Tunnel service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Tunnel;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Set up the environment and start the Tunnel service.
 @param hostName The host name for the network data source.
 @param hostPort The port for the network data source.
 @param argv The arguments to be used with the Tunnel service.
 @param tag The modifier for the service name and port names.
 @param serviceEndpointName The YARP name to be assigned to the new service.
 @param servicePortNumber The port being used by the service.
 @param reportOnExit @c true if service metrics are to be reported on exit and @c false otherwise. */
static void setUpAndGo(const yarp::os::ConstString & hostName,
                       const int                     hostPort,
                       char * *                      argv,
                       const yarp::os::ConstString & tag,
                       const yarp::os::ConstString & serviceEndpointName,
                       const yarp::os::ConstString & servicePortNumber,
                       const bool                    reportOnExit)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("hostName = ", hostName, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_L1("hostPort = ", hostPort); //####
    OD_LOG_P1("argv = ", argv); //####
    OD_LOG_B1("reportOnExit = ", reportOnExit); //####
    TunnelService * stuff = new TunnelService(hostName, hostPort, *argv, tag, serviceEndpointName,
                                              servicePortNumber);
    
    if (stuff)
    {
        if (stuff->start())
        {
            yarp::os::ConstString channelName(stuff->getEndpoint().getName());
            
            OD_LOG_S1s("channelName = ", channelName); //####
            if (RegisterLocalService(channelName, *stuff))
            {
                StartRunning();
                SetSignalHandlers(SignalRunningStop);
                stuff->startPinger();
                for ( ; IsRunning(); )
                {
#if defined(MpM_MainDoesDelayNotYield)
                    yarp::os::Time::delay(ONE_SECOND_DELAY / 10.0);
#else // ! defined(MpM_MainDoesDelayNotYield)
                    yarp::os::Time::yield();
#endif // ! defined(MpM_MainDoesDelayNotYield)
                }
                UnregisterLocalService(channelName, *stuff);
                if (reportOnExit)
                {
                    yarp::os::Bottle metrics;
                    
                    stuff->gatherMetrics(metrics);
                    yarp::os::ConstString converted(Utilities::ConvertMetricsToString(metrics));
                    
                    cout << converted.c_str() << endl;
                }
                stuff->stop();
            }
            else
            {
                OD_LOG("! (RegisterLocalService(channelName, *stuff))"); //####
#if MAC_OR_LINUX_
                GetLogger().fail("Service could not be registered.");
#else // ! MAC_OR_LINUX_
                cerr << "Service could not be registered." << endl;
#endif // ! MAC_OR_LINUX_
            }
        }
        else
        {
            OD_LOG("! (stuff->start())"); //####
#if MAC_OR_LINUX_
            GetLogger().fail("Service could not be started.");
#else // ! MAC_OR_LINUX_
            cerr << "Service could not be started." << endl;
#endif // ! MAC_OR_LINUX_
        }
        delete stuff;
    }
    else
    {
        OD_LOG("! (stuff)"); //####
    }
    OD_LOG_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the Tunnel service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Tunnel service.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
#if defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionWriteToStderr | kODLoggingOptionEnableThreadSupport); //####
#else // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport); //####
#endif // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(*argv);
#endif // MAC_OR_LINUX_
    try
    {
        bool                  nameWasSet = false; // not used
        bool                  reportOnExit = false;
        yarp::os::ConstString serviceEndpointName;
        yarp::os::ConstString servicePortNumber;
        yarp::os::ConstString tag;
        StringVector          arguments;
        
        if (ProcessStandardServiceOptions(argc, argv, T_(" hostname port\n\n"
                                                         "  hostname   IP address to provide "
                                                         "access to\n"
                                                         "  port       port to provide access "
                                                         "to"),
                                          DEFAULT_TUNNEL_SERVICE_NAME, 2015,
                                          STANDARD_COPYRIGHT_NAME, nameWasSet, reportOnExit, tag,
                                          serviceEndpointName, servicePortNumber, kSkipNone,
                                          &arguments))
        {
			Utilities::SetUpGlobalStatusReporter();
			Utilities::CheckForNameServerReporter();
#if CheckNetworkWorks_
            if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))
#endif // CheckNetworkWorks_
            {
                yarp::os::Network yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure
                
                Initialize(*argv);
                if (2 <= arguments.size())
                {
                    struct in_addr        addrBuff;
                    int                   hostPort = -1;
                    yarp::os::ConstString hostName;
                    const char *          startPtr = arguments[1].c_str();
                    char *                endPtr;
                    int                   tempInt = static_cast<int>(strtol(startPtr, &endPtr, 10));

                    hostName = arguments[0];
                    OD_LOG_S1s("hostName <- ", hostName); //####
                    if ((0 < inet_pton(AF_INET, hostName.c_str(), &addrBuff)) &&
                        (startPtr != endPtr) && (! *endPtr) && (0 < tempInt))
                    {
                        // Useable data.
                        hostPort = tempInt;
                    }
                    if ((0 < hostPort) && (0 < hostName.size()))
                    {
                        setUpAndGo(hostName, hostPort, argv, tag, serviceEndpointName,
                                   servicePortNumber, reportOnExit);
                    }
                    else
                    {
#if MAC_OR_LINUX_
                        GetLogger().fail("Invalid argument(s).");
#else // ! MAC_OR_LINUX_
                        cerr << "Invalid argument(s)." << endl;
#endif // ! MAC_OR_LINUX_
                    }
                }
                else
                {
#if MAC_OR_LINUX_
                    GetLogger().fail("Missing argument(s).");
#else // ! MAC_OR_LINUX_
                    cerr << "Missing argument(s)." << endl;
#endif // ! MAC_OR_LINUX_
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
			Utilities::ShutDownGlobalStatusReporter();
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
