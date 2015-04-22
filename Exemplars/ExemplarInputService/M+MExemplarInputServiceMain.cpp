//--------------------------------------------------------------------------------------------------
//
//  File:       M+MExemplarInputServiceMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the exemplar input service.
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
//  Created:    2014-09-15
//
//--------------------------------------------------------------------------------------------------

#include "M+MExemplarInputService.h"

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
 @brief The main application for the exemplar input service. */

/*! @dir ExemplarInputService
 @brief The set of files that implement the exemplar input service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Exemplar;
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

/*! @brief Display the available commands. */
static void displayCommands(void)
{
    OD_LOG_ENTER(); //####
    cout << "Commands:" << endl;
    cout << "  ? - display this list" << endl;
    cout << "  b - start (begin) the output stream," << endl;
    cout << "  c - configure the service" << endl;
    cout << "  e - stop (end) the output stream" << endl;
    cout << "  q - quit the application" << endl;
    cout << "  r - restart the output stream" << endl;
    cout << "  u - reset the configuration (unconfigure) so that it will be reprocessed" << endl;
    OD_LOG_EXIT(); //####
} // displayCommands

/*! @brief Set up the environment and start the exemplar input service.
 @param burstPeriod The burst period in seconds.
 @param burstSize The number of random values to generate in each burst.
 @param argv The arguments to be used with the exemplar input service.
 @param tag The modifier for the service name and port names.
 @param serviceEndpointName The YARP name to be assigned to the new service.
 @param servicePortNumber The port being used by the service.
 @param autostartWasSet @c true if the service is to be started immediately.
 @param stdinAvailable @c true if running in the foreground and @c false otherwise.
 @param reportOnExit @c true if service metrics are to be reported on exit and @c false otherwise. */
static void setUpAndGo(double &                      burstPeriod,
                       int &                         burstSize,
                       char * *                      argv,
                       const yarp::os::ConstString & tag,
                       const yarp::os::ConstString & serviceEndpointName,
                       const yarp::os::ConstString & servicePortNumber,
                       const bool                    autostartWasSet,
                       const bool                    stdinAvailable,
                       const bool                    reportOnExit)
{
    OD_LOG_ENTER(); //####
    OD_LOG_D1("burstPeriod = ", burstPeriod); //####
    OD_LOG_L1("burstSize = ", burstSize); //####
    OD_LOG_P1("argv = ", argv); //####
    OD_LOG_S3s("tag = ", tag, "serviceEndpointName = ", serviceEndpointName, //####
               "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_B3("autostartWasSet = ", autostartWasSet, "stdinAvailable = ", stdinAvailable, //####
              "reportOnExit = ", reportOnExit); //####
    ExemplarInputService * stuff = new ExemplarInputService(*argv, tag, serviceEndpointName,
                                                            servicePortNumber);

    if (stuff)
    {
        double tempDouble;
        int    tempInt;
        
        if (stuff->start())
        {
            yarp::os::ConstString channelName(stuff->getEndpoint().getName());
            
            OD_LOG_S1s("channelName = ", channelName); //####
            if (RegisterLocalService(channelName, *stuff))
            {
                bool             configured = false;
                yarp::os::Bottle configureData;
                
                StartRunning();
                SetSignalHandlers(SignalRunningStop);
                stuff->startPinger();
                if (autostartWasSet || (! stdinAvailable))
                {
                    configureData.addDouble(burstPeriod);
                    configureData.addInt(burstSize);
                    if (stuff->configure(configureData))
                    {
                        stuff->startStreams();
                    }
                }
                for ( ; IsRunning(); )
                {
                    if ((! autostartWasSet) && stdinAvailable)
                    {
                        char inChar;
                        
                        cout << "Operation: [? b c e q r u]? ";
                        cout.flush();
                        cin >> inChar;
                        switch (inChar)
                        {
                            case '?' :
                                // Help
                                displayCommands();
                                break;
                                
                            case 'b' :
                            case 'B' :
                                // Start streams
                                if (! configured)
                                {
                                    configureData.clear();
                                    configureData.addDouble(burstPeriod);
                                    configureData.addInt(burstSize);
                                    if (stuff->configure(configureData))
                                    {
                                        configured = true;
                                    }
                                }
                                if (configured)
                                {
                                    stuff->startStreams();
                                }
                                break;
                                
                            case 'c' :
                            case 'C' :
                                // Configure
                                cout << "Burst size: ";
                                cout.flush();
                                cin >> tempInt;
                                cout << "Burst period: ";
                                cout.flush();
                                cin >> tempDouble;
                                if ((0 < tempInt) && (0 < tempDouble))
                                {
                                    burstPeriod = tempDouble;
                                    burstSize = tempInt;
                                    configureData.clear();
                                    configureData.addDouble(burstPeriod);
                                    configureData.addInt(burstSize);
                                    if (stuff->configure(configureData))
                                    {
                                        configured = true;
                                    }
                                }
                                else
                                {
                                    cout << "One or both values out of range." << endl;
                                }
                                break;
                                
                            case 'e' :
                            case 'E' :
                                // Stop streams
                                stuff->stopStreams();
                                break;
                                
                            case 'q' :
                            case 'Q' :
                                // Quit
                                StopRunning();
                                break;
                                
                            case 'r' :
                            case 'R' :
                                // Restart streams
                                if (! configured)
                                {
                                    configureData.clear();
                                    configureData.addDouble(burstPeriod);
                                    configureData.addInt(burstSize);
                                    if (stuff->configure(configureData))
                                    {
                                        configured = true;
                                    }
                                }
                                if (configured)
                                {
                                    stuff->restartStreams();
                                }
                                break;
                                
                            case 'u' :
                            case 'U' :
                                // Unconfigure
                                configured = false;
                                break;
                                
                            default :
                                cout << "Unrecognized request '" << inChar << "'." << endl;
                                break;
                                
                        }
                    }
                    else
                    {
#if defined(MpM_MainDoesDelayNotYield)
                        yarp::os::Time::delay(ONE_SECOND_DELAY / 10.0);
#else // ! defined(MpM_MainDoesDelayNotYield)
                        yarp::os::Time::yield();
#endif // ! defined(MpM_MainDoesDelayNotYield)
                    }
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

/*! @brief The entry point for running the exemplar input service.
 
 The second, optional, argument is the port number to be used and the first, optional, argument is
 the name of the channel to be used. There is no output.
 The option 'p' specifies the burst period, in seconds, while the option 's' specifies the number of
 random values to generate in each burst.
 The option 'r' indicates that the service metrics are to be reported on exit.
 The option 't' specifies the tag modifier, which is applied to the name of the channel, if the
 name was not specified. It is also applied to the service name as a suffix.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the exemplar input service.
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
        bool                  autostartWasSet = false;
        bool                  nameWasSet = false; // not used
        bool                  reportOnExit = false;
        bool                  stdinAvailable = CanReadFromStandardInput();
        double                burstPeriod = 1;
        int                   burstSize = 1;
        yarp::os::ConstString serviceEndpointName;
        yarp::os::ConstString servicePortNumber;
        yarp::os::ConstString tag;
        StringVector          arguments;
        
        if (ProcessStandardServiceOptions(argc, argv, T_(" [period [size]]\n\n"
                                                         "  period     Optional interval between "
                                                         "bursts\n"
                                                         "  size       Optional burst size"),
                                          DEFAULT_EXEMPLARINPUT_SERVICE_NAME, 2014,
                                          STANDARD_COPYRIGHT_NAME, autostartWasSet, nameWasSet,
                                          reportOnExit, tag, serviceEndpointName, servicePortNumber,
                                          kSkipNone, &arguments))
        {
            Utilities::CheckForNameServerReporter();
#if CheckNetworkWorks_
            if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))
#endif // CheckNetworkWorks_
            {
                yarp::os::Network yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure
                
                Initialize(*argv);
                if (0 < arguments.size())
                {
                    const char * startPtr = arguments[0].c_str();
                    char *       endPtr;
                    double       tempDouble;
                    
                    // 1 or more arguments
                    tempDouble = strtod(startPtr, &endPtr);
                    if ((startPtr != endPtr) && (! *endPtr) && (0 < tempDouble))
                    {
                        // Useable data.
                        burstPeriod = tempDouble;
                    }
                    if (1 < arguments.size())
                    {
                        int tempInt;
                        
                        // 2 or more arguments
                        startPtr = arguments[1].c_str();
                        tempInt = static_cast<int>(strtol(startPtr, &endPtr, 10));
                        if ((startPtr != endPtr) && (! *endPtr) && (0 < tempInt))
                        {
                            // Useable data.
                            burstSize = tempInt;
                        }
                    }
                }
                setUpAndGo(burstPeriod, burstSize, argv, tag, serviceEndpointName,
                           servicePortNumber, autostartWasSet, stdinAvailable, reportOnExit);
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
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
