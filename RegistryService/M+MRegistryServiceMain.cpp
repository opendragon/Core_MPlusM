//--------------------------------------------------------------------------------------------------
//
//  File:       M+MRegistryServiceMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the Service Registry M+M service.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#include "M+MRegistryService.h"
#include "M+MNameServerReportingThread.h"

#include <mpm/M+MRequests.h>
#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file 
 @brief The main application for the M+M Registry Service. */

/*! @dir RegistryService
 @brief The set of files that implement the Registry Service application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief Set to @c true to use an in-memory database and @c false to use a disk-based database. */
#if defined(MpM_UseDiskDatabase)
# define USE_INMEMORY false
#else // ! defined(MpM_UseDiskDatabase)
# define USE_INMEMORY true
#endif // ! defined(MpM_UseDiskDatabase)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the Registry Service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Registry Service.
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
        bool                  reportOnExit = false;
        yarp::os::ConstString serviceEndpointName; // not used
        yarp::os::ConstString servicePortNumber;
        yarp::os::ConstString tag; // not used
        
        ProcessStandardServiceOptions(argc, argv, MpM_REGISTRY_ENDPOINT_NAME, reportOnExit, tag,
                                      serviceEndpointName, servicePortNumber);
        // Note - no call to Utilities::CheckForNameServerReporter(), since this code sets up the
        // NameServerReporter!
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))
#endif // CheckNetworkWorks_
        {
            yarp::os::Network yarp; // This is necessary to establish any connections to the YARP
                                    // infrastructure
            
            Initialize(*argv);
            Registry::RegistryService * stuff = new Registry::RegistryService(*argv, USE_INMEMORY,
                                                                              servicePortNumber);
            if (stuff)
            {
                stuff->enableMetrics();
                if (stuff->start())
                {
                    // Note that the Registry Service is self-registering... so we don't need to
                    // call RegisterLocalService() _or_ start a 'pinger'.
                    Registry::NameServerReportingThread * reporter = new
                                                                Registry::NameServerReportingThread;
                    
                    StartRunning();
                    SetSignalHandlers(SignalRunningStop);
                    stuff->startChecker();
                    reporter->start();
                    for ( ; IsRunning(); )
                    {
#if defined(MpM_MainDoesDelayNotYield)
                        yarp::os::Time::delay(ONE_SECOND_DELAY);
#else // ! defined(MpM_MainDoesDelayNotYield)
                        yarp::os::Time::yield();
#endif // ! defined(MpM_MainDoesDelayNotYield)
                    }
                    reporter->stop();
                    for ( ; reporter->isRunning(); )
                    {
                        yarp::os::Time::delay(PING_CHECK_INTERVAL / 3.1);
                    }
                    delete reporter;
                    if (reportOnExit)
                    {
                        yarp::os::Bottle metrics;
                        
                        stuff->gatherMetrics(metrics);
                        yarp::os::ConstString converted(Utilities::ConvertMetricsToString(metrics));
                        
                        std::cout << converted.c_str() << std::endl;
                    }
                    stuff->stop();
                }
                else
                {
                    OD_LOG("! (stuff->start())"); //####
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
            std::cerr << "YARP network not running." << std::endl;
# endif // ! MAC_OR_LINUX_
        }
#endif // CheckNetworkWorks_
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
