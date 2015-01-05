//--------------------------------------------------------------------------------------------------
//
//  File:       M+MJavaScriptServiceMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the JavaScript Input/Output service.
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
//  Created:    2015-01-05
//
//--------------------------------------------------------------------------------------------------

#include "M+MJavaScriptService.h"

#include <mpm/M+MEndpoint.h>
#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if (! MAC_OR_LINUX_) //ASSUME WINDOWS
# include <mpm/getopt.h>
#endif //(! MAC_OR_LINUX_)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Winvalid-offsetof"
#endif // defined(__APPLE__)
#include <jsapi.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The main application for the JavaScript Input/Output service. */

/*! @dir JavaScriptService
 @brief The set of files that implement the JavaScript Input/Output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::JavaScript;
using std::cin;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The accepted command line arguments for the service. */
#define JAVASCRIPT_OPTIONS "rt:"

/*! @brief The number of megabytes before the JavaScript engine triggers a garbage collection. */
#define JAVASCRIPT_GC_SIZE 16

/*! @brief The number of bytes for each JavaScript 'stack chunk'. */
#define JAVASCRIPT_STACKCHUNK_SIZE 8192

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Display the available commands. */
static void displayCommands(void)
{
    OD_LOG_ENTER(); //####
    cout << "Commands:" << endl;
    cout << "  ? - display this list" << endl;
    cout << "  b - start (begin) the input and output streams" << endl;
    cout << "  c - configure the service" << endl;
    cout << "  e - stop (end) the input and output streams" << endl;
    cout << "  q - quit the application" << endl;
    cout << "  r - restart the input and output streams" << endl;
    cout << "  u - reset the configuration (unconfigure) so that it will be reprocessed" << endl;
    OD_LOG_EXIT(); //####
} // displayCommands

/*! @brief The error reporter callback for the JavaScript engine.
 @param cx The context in which the error happened.
 @param message An error message.
 @param report An error report record containing additional details about the error. */
static void reportJavaScriptError(JSContext *     cx,
                                  const char *    message,
                                  JSErrorReport * report)
{
    // Note that, since this is a callback for the JavaScript engine, it must NOT throw any C++
    // exceptions!
    try
    {
        std::cerr << (report->filename ? report->filename : "[no filename]") << ":" <<
                    report->lineno << message << endl;
    }
    catch (...)
    {
        // Suppress any C++ exception caused by this function.
    }
} // reportJavaScriptError

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the JavaScript Input/Output service.
 
 The second, optional, argument is the port number to be used and the first, optional, argument is
 the name of the channel to be used. There is no output.
 The option 't' specifies the tag modifier, which is applied to the name of the channel, if the
 name was not specified. It is also applied to the service name as a suffix.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example service.
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
        bool                  stdinAvailable = CanReadFromStandardInput();
        yarp::os::ConstString tag;
        
        opterr = 0; // Suppress the error message resulting from an unknown option.
        for (int cc = getopt(argc, argv, JAVASCRIPT_OPTIONS); -1 != cc;
             cc = getopt(argc, argv, JAVASCRIPT_OPTIONS))
        {
            switch (cc)
            {
                case 'r' :
                    // Report metrics on exit
                    reportOnExit = true;
                    break;
                    
                case 't' :
                    // Tag
                    tag = optarg;
                    OD_LOG_S1s("tag <- ", tag); //####
                    break;
                    
                default :
                    // Ignore unknown options.
                    break;
                    
            }
        }
        Utilities::CheckForNameServerReporter();
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))
#endif // CheckNetworkWorks_
        {
            yarp::os::Network     yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure
            yarp::os::ConstString serviceEndpointName;
            yarp::os::ConstString servicePortNumber;
            
            Initialize(*argv);
            if (optind >= argc)
            {
                if (0 < tag.size())
                {
                    serviceEndpointName = yarp::os::ConstString(DEFAULT_JAVASCRIPT_SERVICE_NAME) +
                                            "/" + tag;
                }
                else
                {
                    serviceEndpointName = DEFAULT_JAVASCRIPT_SERVICE_NAME;
                }
            }
            else if ((optind + 1) == argc)
            {
                serviceEndpointName = argv[optind];
            }
            else
            {
                // 2 args
                serviceEndpointName = argv[optind];
                servicePortNumber = argv[optind + 1];
            }
            if (JS_Init())
            {
                JSContext * jct = NULL;
                JSRuntime * jrt = JS_NewRuntime(JAVASCRIPT_GC_SIZE * 1024 * 1024);
                
                if (jrt)
                {
                    jct = JS_NewContext(jrt, JAVASCRIPT_STACKCHUNK_SIZE);
                    if (jct)
                    {
                        JS_SetErrorReporter(jrt, reportJavaScriptError);
                        // Note that JS_SetOptions() is no longer supported.
//                        JS_SetOptions(jct, JSOPTION_VAROBJFIX | JSOPTION_EXTRA_WARNINGS);
                    }
                    else
                    {
                        OD_LOG("! (jct)"); //####
#if MAC_OR_LINUX_
                        GetLogger().fail("JavaScript context could not be allocated.");
#else // ! MAC_OR_LINUX_
                        std::cerr << "JavaScript context could not be allocated." << std::endl;
#endif // ! MAC_OR_LINUX_
                        JS_DestroyRuntime(jrt);
                        jrt = NULL;
                    }
                }
                else
                {
                    OD_LOG("! (jrt)"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("JavaScript runtime could not be allocated.");
#else // ! MAC_OR_LINUX_
                    std::cerr << "JavaScript runtime could not be allocated." << std::endl;
#endif // ! MAC_OR_LINUX_
                }
                if (jrt && jct)
                {
                    JavaScriptService * stuff = new JavaScriptService(jct, *argv, tag,
                                                                      serviceEndpointName,
                                                                      servicePortNumber);
                    
                    if (stuff)
                    {
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
                                if (! stdinAvailable)
                                {
                                    if (stuff->configure(configureData))
                                    {
                                        stuff->startStreams();
                                    }
                                }
                                for ( ; IsRunning(); )
                                {
                                    if (stdinAvailable)
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
                                                // Configure - nothing to do for the JavaScript
                                                // Input/Output service.
                                                if (stuff->configure(configureData))
                                                {
                                                    configured = true;
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
                                                cout << "Unrecognized request '" << inChar <<
                                                        "'." << endl;
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
                                    yarp::os::ConstString converted =
                                    Utilities::ConvertMetricsToString(metrics);
                                    
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
                                std::cerr << "Service could not be registered." << std::endl;
#endif // ! MAC_OR_LINUX_
                            }
                        }
                        else
                        {
                            OD_LOG("! (stuff->start())"); //####
#if MAC_OR_LINUX_
                            GetLogger().fail("Service could not be started.");
#else // ! MAC_OR_LINUX_
                            std::cerr << "Service could not be started." << std::endl;
#endif // ! MAC_OR_LINUX_
                        }
                        delete stuff;
                    }
                    else
                    {
                        OD_LOG("! (stuff)"); //####
                    }
                    JS_DestroyContext(jct);
                    JS_DestroyRuntime(jrt);
                }
                JS_ShutDown();
            }
            else
            {
                OD_LOG("! (JS_Init())"); //####
#if MAC_OR_LINUX_
                GetLogger().fail("JavaScript engine could not be started.");
#else // ! MAC_OR_LINUX_
                std::cerr << "JavaScript engine could not be started." << std::endl;
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
