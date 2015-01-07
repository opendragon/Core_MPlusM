//--------------------------------------------------------------------------------------------------
//
//  File:       M+MJavaScriptServiceMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the JavaScript input / output service.
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

#if defined(MAC_OR_LINUX_)
# include <libgen.h>
#else  // ! defined(MAC_OR_LINUX_)
# include <stdlib.h>
#endif // ! defined(MAC_OR_LINUX_)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Winvalid-offsetof"
#endif // defined(__APPLE__)
#include <js/RequiredDefines.h>
#include <jsapi.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the %JavaScript input / output service. */

/*! @dir JavaScript
 @brief The set of files that implement the %JavaScript input / output service. */
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

/*! @brief The number of megabytes before the %JavaScript engine triggers a garbage collection. */
#define JAVASCRIPT_GC_SIZE 16

/*! @brief The number of bytes for each %JavaScript 'stack chunk'. */
#define JAVASCRIPT_STACKCHUNK_SIZE 8192

// The class of the global object.
static JSClass lGlobalClass =
{
    "global",
    JSCLASS_GLOBAL_FLAGS,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    JS_GlobalObjectTraceHook
};

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

/*! @brief The error reporter callback for the %JavaScript engine.
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

/*! @brief Set up the %JavaScript environment and start the %JavaScript input / output service.
 @param jct The %JavaScript engine context.
 @param script The %JavaScript source code to be executed.
 @param argv The arguments to be used with the %JavaScript input / output service.
 @param tag The modifier for the service name and port names.
 @param serviceEndpointName The YARP name to be assigned to the new service.
 @param servicePortNumber The port being used by the service.
 @param stdinAvailable @c true if running in the foreground and @c false otherwise.
 @param reportOnExit @c true if service metrics are to be reported on exit and @c false otherwise.
 */
static void setUpAndGo(JSContext *                   jct,
                       const yarp::os::ConstString & script,
                       char * *                      argv,
                       const yarp::os::ConstString & tag,
                       const yarp::os::ConstString & serviceEndpointName,
                       const yarp::os::ConstString & servicePortNumber,
                       const bool                    stdinAvailable,
                       const bool                    reportOnExit)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "argv = ", argv); //####
    OD_LOG_S3s("tag = ", tag, "serviceEndpointName = ", serviceEndpointName, //####
               "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_B2("stdinAvailable = ", stdinAvailable, "reportOnExit = ", reportOnExit); //####
    // Enter a request before running anything in the context.
    JSAutoRequest    ar(jct);
    JS::RootedObject global(jct, JS_NewGlobalObject(jct, &lGlobalClass, NULL,
                                                    JS::FireOnNewGlobalHook));
    
    if (global)
    {
        // Enter the new global object's compartment.
        JSAutoCompartment ac(jct, global);
        
        // Populate the global object with the standard globals, like Object and Array.
        if (JS_InitStandardClasses(jct, global))
        {
            JavaScriptService * stuff = new JavaScriptService(jct, *argv, tag, serviceEndpointName,
                                                              servicePortNumber);
            
            if (stuff)
            {
                if (stuff->start())
                {
                    yarp::os::ConstString channelName =
                    stuff->getEndpoint().getName();
                    
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
                                        // Configure - nothing to do for the JavaScript input /
                                        // output service.
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
        }
        else
        {
            OD_LOG("! (JS_InitStandardClasses(jct, global))"); //####
#if MAC_OR_LINUX_
            GetLogger().fail("JavaScript global object could not be initialized.");
#else // ! MAC_OR_LINUX_
            std::cerr << "JavaScript global object could not be initialized." << std::endl;
#endif // ! MAC_OR_LINUX_
        }
    }
    else
    {
        OD_LOG("! (global)"); //####
#if MAC_OR_LINUX_
        GetLogger().fail("JavaScript global object could not be created.");
#else // ! MAC_OR_LINUX_
        std::cerr << "JavaScript global object could not be created." << std::endl;
#endif // ! MAC_OR_LINUX_
    }
    OD_LOG_EXIT(); //####
} // setUpAndGo

/*! @brief Return the file name part of a path.
 @param inFileName The file path to be processed.
 @returns The file name part of a path. */
static yarp::os::ConstString getFileNamePart(const yarp::os::ConstString & inFileName)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inFileName = ", inFileName); //####
    yarp::os::ConstString result;
#if defined(MAC_OR_LINUX_)
    char * nameCopy = strdup(inFileName.c_str());
#else // ! defined(MAC_OR_LINUX_)
    char   baseFileName[_MAX_FNAME + 10];
    char   baseExtension[_MAX_EXT + 10];
#endif // ! defined(MAC_OR_LINUX_)
    
#if defined(MAC_OR_LINUX_)
    result = basename(nameCopy);
    free(nameCopy);
#else // ! defined(MAC_OR_LINUX_)
    _splitpath(inFileName.c_str(), NULL, NULL, baseFileName, baseExtension);
    result = baseFileName;
    result += ".";
    result += baseExtension;
#endif // ! defined(MAC_OR_LINUX_)
    OD_LOG_EXIT_S(result.c_str()); //####
    return result;
} // getFileNamePart

/*! @brief Return the base name of a file name.
 @param inFileName The file name to be processed.
 @returns The base name of a file name. */
static yarp::os::ConstString getFileNameBase(const yarp::os::ConstString & inFileName)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inFileName = ", inFileName);
    yarp::os::ConstString result;
    size_t                index = inFileName.rfind('.');
    
    if (yarp::os::ConstString::npos == index)
    {
        result = inFileName;
    }
    else
    {
        result = inFileName.substr(0, index);
    }
    OD_LOG_EXIT_S(result.c_str()); //####
    return result;
} // getFileNameBase

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the %JavaScript input / output service.
 
 The first argument is the path of the script to be run by the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the %JavaScript input / output service.
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
        bool                  nameWasSet;
        bool                  stdinAvailable = CanReadFromStandardInput();
        yarp::os::ConstString serviceEndpointName;
        yarp::os::ConstString servicePortNumber;
        yarp::os::ConstString tag;
        
        nameWasSet = ProcessStandardServiceOptions(argc, argv, DEFAULT_JAVASCRIPT_SERVICE_NAME,
                                                   reportOnExit, tag, serviceEndpointName,
                                                   servicePortNumber);
        Utilities::CheckForNameServerReporter();
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))
#endif // CheckNetworkWorks_
        {
            yarp::os::Network yarp; // This is necessary to establish any connections to the YARP
                                    // infrastructure
            
            Initialize(*argv);
            if (optind < argc)
            {
                yarp::os::ConstString scriptPath(argv[optind]);
                yarp::os::ConstString scriptSource;
                
                if (! nameWasSet)
                {
                    yarp::os::ConstString tagModifier(getFileNameBase(getFileNamePart(scriptPath)));

                    serviceEndpointName += "/" + tagModifier;
                }
                // Make sure that the scriptPath is valid and construct the modified 'tag' and
                // (optional) endpoint name.
                FILE * scratch = fopen(scriptPath.c_str(), "r");
                
                if (scratch)
                {
                    // The path given is a readable file, so read it in and prepare to start the
                    // service by filling in the scriptSource.
                    char   buffer[10240];
                    size_t numRead;
                    
                    for ( ; ! feof(scratch); )
                    {
                        numRead = fread(buffer, 1, sizeof(buffer) - 1, scratch);
                        if (numRead)
                        {
                            buffer[numRead] = '\0';
                            scriptSource += buffer;
                        }
                    }
                    fclose(scratch);
                }
                if (0 < scriptSource.size())
                {
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
                                std::cerr << "JavaScript context could not be allocated." <<
                                            std::endl;
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
                            setUpAndGo(jct, scriptSource, argv, tag, serviceEndpointName,
                                       servicePortNumber, stdinAvailable, reportOnExit);
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
                else
                {
                    OD_LOG("! (0 < scriptSource.size())"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("Empty script file.");
#else // ! MAC_OR_LINUX_
                    std::cerr << "Empty script file." << std::endl;
#endif // ! MAC_OR_LINUX_
                }
            }
            else
            {
# if MAC_OR_LINUX_
                GetLogger().fail("Missing script file path.");
# else // ! MAC_OR_LINUX_
                std::cerr << "Missing script file path." << std::endl;
# endif // ! MAC_OR_LINUX_
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
