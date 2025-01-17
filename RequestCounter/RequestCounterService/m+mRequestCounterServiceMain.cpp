//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRequestCounterServiceMain.cpp
//
//  Project:    m+m
//
//  Contains:   The main application for the Request Counter service.
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
//  Created:    2014-03-14
//
//--------------------------------------------------------------------------------------------------

#include "m+mRequestCounterService.hpp"

#include <m+m/m+mEndpoint.hpp>
#include <m+m/m+mUtilities.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the Request Counter service. */

/*! @dir RequestCounter
 @brief The set of files that support counting requests. */

/*! @dir RequestCounterCommon
 @brief The set of files that are shared between the Request Counter client and Request Counter
 service. */

/*! @dir RequestCounterService
 @brief The set of files that implement the Request Counter service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::RequestCounter;
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

/*! @brief Set up the environment and start the Request Counter service.
 @param[in] progName The path to the executable.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the Request Counter service.
 @param[in] serviceEndpointName The YARP name to be assigned to the new service.
 @param[in] servicePortNumber The port being used by the service.
 @param[in] reportOnExit @c true if service metrics are to be reported on exit and @c false
 otherwise. */
static void
setUpAndGo(const YarpString & progName,
           const int          argc,
           char * *           argv,
           const YarpString & serviceEndpointName,
           const YarpString & servicePortNumber,
           const bool         reportOnExit)
{
    ODL_ENTER(); //####
    ODL_S3s("progName = ", progName, "serviceEndpointName = ", serviceEndpointName, //####
            "servicePortNumber = ", servicePortNumber); //####
    ODL_I1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    ODL_B1("reportOnExit = ", reportOnExit); //####
    RequestCounterService * aService = new RequestCounterService(progName, argc, argv,
                                                                 serviceEndpointName,
                                                                 servicePortNumber);

    if (aService)
    {
        if (aService->startService())
        {
            YarpString channelName(aService->getEndpoint().getName());

            ODL_S1s("channelName = ", channelName); //####
            if (RegisterLocalService(channelName, *aService))
            {
                StartRunning();
                SetSignalHandlers(SignalRunningStop);
                aService->startPinger();
                IdleUntilNotRunning();
                UnregisterLocalService(channelName, *aService);
                if (reportOnExit)
                {
                    yarp::os::Bottle metrics;

                    aService->gatherMetrics(metrics);
                    YarpString converted(Utilities::ConvertMetricsToString(metrics));

                    cout << converted.c_str() << endl;
                }
                aService->stopService();
            }
            else
            {
                ODL_LOG("! (RegisterLocalService(channelName, *aService))"); //####
                MpM_FAIL_(MSG_SERVICE_NOT_REGISTERED);
            }
        }
        else
        {
            ODL_LOG("! (aService->startService())"); //####
            MpM_FAIL_(MSG_SERVICE_NOT_STARTED);
        }
        delete aService;
    }
    else
    {
        ODL_LOG("! (aService)"); //####
    }
    ODL_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the Request Counter service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the Request Counter service.
 @return @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
    YarpString progName(*argv);

#if defined(MpM_ServicesLogToStandardError)
    ODL_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionWriteToStderr | //####
             kODLoggingOptionEnableThreadSupport); //####
#else // ! defined(MpM_ServicesLogToStandardError)
    ODL_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport); //####
#endif // ! defined(MpM_ServicesLogToStandardError)
    ODL_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(progName);
#endif // MAC_OR_LINUX_
    try
    {
        AddressTagModifier          modFlag = kModificationNone;
        bool                        goWasSet = false; // not used
        bool                        reportEndpoint = false;
        bool                        reportOnExit = false;
        YarpString                  serviceEndpointName; // not used
        YarpString                  servicePortNumber;
        YarpString                  tag; // not used
        Utilities::DescriptorVector argumentList;

        if (ProcessStandardServiceOptions(argc, argv, argumentList,
                                          REQUESTCOUNTER_SERVICE_DESCRIPTION_, "", 2014,
                                          STANDARD_COPYRIGHT_NAME_, goWasSet, reportEndpoint,
                                          reportOnExit, tag, serviceEndpointName, servicePortNumber,
                                          modFlag, static_cast<OptionsMask>(kSkipGoOption |
                                                                            kSkipEndpointOption |
                                                                            kSkipModOption |
                                                                            kSkipTagOption)))
        {
            Utilities::SetUpGlobalStatusReporter();
            Utilities::CheckForNameServerReporter();
            if (Utilities::CheckForValidNetwork())
            {
                yarp::os::Network yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure

                Initialize(progName);
                AdjustEndpointName(DEFAULT_REQUESTCOUNTER_SERVICE_NAME_, modFlag, tag,
                                   serviceEndpointName);
                if (reportEndpoint)
                {
                    cout << serviceEndpointName.c_str() << endl;
                }
                else if (Utilities::CheckForRegistryService())
                {
                    setUpAndGo(progName, argc, argv, DEFAULT_REQUESTCOUNTER_SERVICE_NAME_,
                               servicePortNumber, reportOnExit);
                }
                else
                {
                    ODL_LOG("! (Utilities::CheckForRegistryService())"); //####
                    MpM_FAIL_(MSG_REGISTRY_NOT_RUNNING);
                }
            }
            else
            {
                ODL_LOG("! (Utilities::CheckForValidNetwork())"); //####
                MpM_FAIL_(MSG_YARP_NOT_RUNNING);
            }
            Utilities::ShutDownGlobalStatusReporter();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    ODL_EXIT_I(0); //####
    return 0;
} // main
