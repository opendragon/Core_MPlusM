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

#include <mpm/M+MAddressArgumentDescriptor.h>
#include <mpm/M+MEndpoint.h>
#include <mpm/M+MPortArgumentDescriptor.h>
#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the %Tunnel service. */

/*! @dir Tunnel
 @brief The set of files that support routing non-YARP data via YARP. */

/*! @dir TunnelCommon
 @brief The set of files that are shared between the %Tunnel client and %Tunnel service. */

/*! @dir TunnelService
 @brief The set of files that implement the %Tunnel service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
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
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Set up the environment and start the %Tunnel service.
 @param hostName The host name for the network data source.
 @param hostPort The port for the network data source.
 @param progName The path to the executable.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the %Tunnel service.
 @param tag The modifier for the service name and port names.
 @param serviceEndpointName The YARP name to be assigned to the new service.
 @param servicePortNumber The port being used by the service.
 @param reportOnExit @c true if service metrics are to be reported on exit and @c false otherwise. */
static void setUpAndGo(const YarpString & hostName,
                       const int          hostPort,
                       const YarpString & progName,
                       const int          argc,
                       char * *           argv,
                       const YarpString & tag,
                       const YarpString & serviceEndpointName,
                       const YarpString & servicePortNumber,
                       const bool         reportOnExit)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("hostName = ", hostName, "progName = ", progName, "tag = ", tag, //####
               "serviceEndpointName = ", serviceEndpointName); //####
    OD_LOG_S1s("servicePortNumber = ", servicePortNumber); //####
    OD_LOG_LL2("hostPort = ", hostPort, "argc = ", argc); //####
    OD_LOG_P1("argv = ", argv); //####
    OD_LOG_B1("reportOnExit = ", reportOnExit); //####
    TunnelService * aService = new TunnelService(hostName, hostPort, progName, argc, argv, tag,
                                                 serviceEndpointName, servicePortNumber);
    
    if (aService)
    {
        if (aService->start())
        {
            YarpString channelName(aService->getEndpoint().getName());
            
            OD_LOG_S1s("channelName = ", channelName); //####
            if (RegisterLocalService(channelName, *aService))
            {
                StartRunning();
                SetSignalHandlers(SignalRunningStop);
                aService->startPinger();
                for ( ; IsRunning(); )
                {
#if defined(MpM_MainDoesDelayNotYield)
                    yarp::os::Time::delay(ONE_SECOND_DELAY / 10.0);
#else // ! defined(MpM_MainDoesDelayNotYield)
                    yarp::os::Time::yield();
#endif // ! defined(MpM_MainDoesDelayNotYield)
                }
                UnregisterLocalService(channelName, *aService);
                if (reportOnExit)
                {
                    yarp::os::Bottle metrics;
                    
                    aService->gatherMetrics(metrics);
                    YarpString converted(Utilities::ConvertMetricsToString(metrics));
                    
                    cout << converted.c_str() << endl;
                }
                aService->stop();
            }
            else
            {
                OD_LOG("! (RegisterLocalService(channelName, *aService))"); //####
#if MAC_OR_LINUX_
                GetLogger().fail("Service could not be registered.");
#else // ! MAC_OR_LINUX_
                cerr << "Service could not be registered." << endl;
#endif // ! MAC_OR_LINUX_
            }
        }
        else
        {
            OD_LOG("! (aService->start())"); //####
#if MAC_OR_LINUX_
            GetLogger().fail("Service could not be started.");
#else // ! MAC_OR_LINUX_
            cerr << "Service could not be started." << endl;
#endif // ! MAC_OR_LINUX_
        }
        delete aService;
    }
    else
    {
        OD_LOG("! (aService)"); //####
    }
    OD_LOG_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the %Tunnel service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the %Tunnel service.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
    YarpString progName(*argv);

#if defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
                kODLoggingOptionIncludeThreadID | kODLoggingOptionWriteToStderr | //####
                kODLoggingOptionEnableThreadSupport); //####
#else // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
                kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport); //####
#endif // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(progName);
#endif // MAC_OR_LINUX_
    try
    {
        bool                                 goWasSet = false; // not used
        bool                                 nameWasSet = false; // not used
        bool                                 reportOnExit = false;
        int                                  hostPort;
        YarpString                           hostName;
        YarpString                           serviceEndpointName;
        YarpString                           servicePortNumber;
        YarpString                           tag;
        Utilities::AddressArgumentDescriptor firstArg("hostname",
                                                      T_("IP address to provide access to"),
                                                      "127.0.0.1", false, &hostName);
        Utilities::PortArgumentDescriptor    secondArg("port", T_("Port to provide access to"),
                                                       12345, false, false, &hostPort);
        Utilities::DescriptorVector          argumentList;

        argumentList.push_back(&firstArg);
        argumentList.push_back(&secondArg);
        if (ProcessStandardServiceOptions(argc, argv, argumentList, DEFAULT_TUNNEL_SERVICE_NAME,
                                          TUNNEL_SERVICE_DESCRIPTION, 2015, STANDARD_COPYRIGHT_NAME,
                                          goWasSet, nameWasSet, reportOnExit, tag,
                                          serviceEndpointName, servicePortNumber, kSkipGoOption))
        {
			Utilities::SetUpGlobalStatusReporter();
			Utilities::CheckForNameServerReporter();
            if (Utilities::CheckForValidNetwork())
            {
                yarp::os::Network yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure
                
                Initialize(progName);
                if (Utilities::CheckForRegistryService())
                {
                    setUpAndGo(hostName, hostPort, progName, argc, argv, tag, serviceEndpointName,
                               servicePortNumber, reportOnExit);
                }
                else
                {
                    OD_LOG("! (Utilities::CheckForRegistryService())"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("Registry Service not running.");
#else // ! MAC_OR_LINUX_
                    cerr << "Registry Service not running." << endl;
#endif // ! MAC_OR_LINUX_
                }
            }
            else
            {
                OD_LOG("! (Utilities::CheckForValidNetwork())"); //####
#if MAC_OR_LINUX_
                GetLogger().fail("YARP network not running.");
#else // ! MAC_OR_LINUX_
                cerr << "YARP network not running." << endl;
#endif // ! MAC_OR_LINUX_
            }
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
