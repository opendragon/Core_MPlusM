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
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file 
 @brief The main application for the M+M %Registry Service. */

/*! @dir RegistryService
 @brief The set of files that implement the %Registry Service application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief Set to @c true to use an in-memory database and @c false to use a disk-based database. */
#if defined(MpM_UseDiskDatabase)
# define USE_INMEMORY_ false
#else // ! defined(MpM_UseDiskDatabase)
# define USE_INMEMORY_ true
#endif // ! defined(MpM_UseDiskDatabase)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Set up the environment and perform the operation.
 @param progName The path to the executable.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the %Registry service.
 @param servicePortNumber The port being used by the service.
 @param reportOnExit @c true if service metrics are to be reported on exit and @c false
 otherwise. */
static void setUpAndGo(const YarpString & progName,
                       const int          argc,
                       char * *           argv,
                       const YarpString & servicePortNumber,
                       const bool         reportOnExit)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("progName = ", progName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_P1("argv = ", argv); //####
    OD_LOG_B1("reportOnExit = ", reportOnExit); //####
    Registry::RegistryService * aService = new Registry::RegistryService(progName, argc, argv,
                                                                         USE_INMEMORY_,
                                                                         servicePortNumber);

    if (aService)
    {
        aService->enableMetrics();
        if (aService->start())
        {
            // Note that the Registry Service is self-registering... so we don't
            // need to call RegisterLocalService() _or_ start a 'pinger'.
            Registry::NameServerReportingThread * reporter =
                                                            new Registry::NameServerReportingThread;
            
            StartRunning();
            SetSignalHandlers(SignalRunningStop);
            aService->startChecker();
            reporter->start();
            for ( ; IsRunning(); )
            {
#if defined(MpM_MainDoesDelayNotYield)
                yarp::os::Time::delay(ONE_SECOND_DELAY_);
#else // ! defined(MpM_MainDoesDelayNotYield)
                yarp::os::Time::yield();
#endif // ! defined(MpM_MainDoesDelayNotYield)
            }
            reporter->stop();
            for ( ; reporter->isRunning(); )
            {
                yarp::os::Time::delay(PING_CHECK_INTERVAL_ / 3.1);
            }
            delete reporter;
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
            OD_LOG("! (aService->start())"); //####
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

/*! @brief The entry point for running the %Registry Service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the %Registry Service.
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
        bool                        goWasSet = false; // not used
        bool                        nameWasSet = false; // not used
        bool                        reportOnExit = false;
        YarpString                  serviceEndpointName; // not used
        YarpString                  servicePortNumber;
        YarpString                  tag; // not used
        Utilities::DescriptorVector argumentList;

		if (ProcessStandardServiceOptions(argc, argv, argumentList, MpM_REGISTRY_ENDPOINT_NAME_,
                                          REGISTRY_SERVICE_DESCRIPTION_, "", 2014,
                                          STANDARD_COPYRIGHT_NAME_, goWasSet, nameWasSet,
                                          reportOnExit, tag, serviceEndpointName, servicePortNumber,
                                          static_cast<OptionsMask>(kSkipGoOption |
                                                                   kSkipEndpointOption |
                                                                   kSkipTagOption)))
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
                    OD_LOG("Utilities::CheckForRegistryService()"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("Registry Service already running.");
#else // ! MAC_OR_LINUX_
                    cerr << "Registry Service already running." << endl;
#endif // ! MAC_OR_LINUX_
                }
                else
                {
                    setUpAndGo(progName, argc, argv, servicePortNumber, reportOnExit);
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
