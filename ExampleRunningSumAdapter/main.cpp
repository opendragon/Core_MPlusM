//--------------------------------------------------------------------------------------
//
//  File:       ExampleRunningSumClient/main.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The main application for the client of a simple Yarp++ service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-03-18
//
//--------------------------------------------------------------------------------------

//#define OD_ENABLE_LOGGING /* */
#include "ODLogging.h"
#include "YPPExampleRunningSumClient.h"
#include "YPPExampleRunningSumControlInputHandler.h"
#include "YPPExampleRunningSumDataInputHandler.h"
#include <iostream>
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#include <yarp/os/all.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace YarpPlusPlusExample;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief Run loop control; @c true if the service is to keep going and @c false otherwise. */
static bool lKeepRunning;

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if (defined(__APPLE__) || defined(__linux__))
/*! @brief The signal handler to catch requests to stop the service.
 @param signal The signal being handled. */
static void stopRunning(int signal)
{
# pragma unused(signal)
    lKeepRunning = false;
} // stopRunning
#endif // defined(__APPLE__) || defined(__linux__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for creating an example client.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID |//####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr);//####
    OD_LOG_ENTER();//####
    try
    {
        if (yarp::os::Network::checkNetwork())
        {
#if (defined(OD_ENABLE_LOGGING) && defined(YPP_DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network::setVerbosity(1);
#else // ! (defined(OD_ENABLE_LOGGING) && defined(YPP_DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network::setVerbosity(-1);
#endif // ! (defined(OD_ENABLE_LOGGING) && defined(YPP_DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
            
            YarpPlusPlus::Initialize();
            ExampleRunningSumClient * stuff = new ExampleRunningSumClient;
            
            if (stuff)
            {
                lKeepRunning = true;
#if (defined(__APPLE__) || defined(__linux__))
                signal(SIGHUP, stopRunning);
                signal(SIGINT, stopRunning);
                signal(SIGINT, stopRunning);
                signal(SIGUSR1, stopRunning);
#endif // defined(__APPLE__) || defined(__linux__)
                if (stuff->findService("Name RunningSum"))
                {
                    if (stuff->connectToService())
                    {
                        yarp::os::Port *                       controlChannel = new yarp::os::Port;
                        yarp::os::Port *                       dataChannel = new yarp::os::Port;
                        yarp::os::Port *                       outputChannel = new yarp::os::Port;
                        ExampleRunningSumControlInputHandler * controlHandler =
                                                                        new ExampleRunningSumControlInputHandler(stuff);
                        ExampleRunningSumDataInputHandler *    dataHandler =
                                                                        new ExampleRunningSumDataInputHandler(stuff);
                        
                        if (controlChannel && dataChannel && outputChannel && controlHandler && dataHandler)
                        {
                            yarp::os::ConstString controlName("/adapter/control/runningsum");
                            yarp::os::ConstString dataName("/adapter/data/runningsum");
                            yarp::os::ConstString outputName("/adapter/output/runningsum");
                            
                            if (argc > 1)
                            {
                                controlName = argv[1];
                                if (argc > 2)
                                {
                                    dataName = argv[2];
                                    if (argc > 3)
                                    {
                                        outputName = argv[3];
                                    }
                                }
                            }
                            if (controlChannel->open(controlName) && dataChannel->open(dataName) &&
                                outputChannel->open(outputName))
                            {
                                controlChannel->setReader(*controlHandler);
                                dataChannel->setReader(*dataHandler);
                                
                                
                                
                                
                                
                                
                            }
                            else
                            {
                                OD_LOG("! (controlChannel->open(controlName) && dataChannel->open(dataName) && "//####
                                       "outputChannel->open(outputName))");//####
                                cerr << "Problem opening a port." << endl;
                            }
                            controlChannel->close();
                            dataChannel->close();
                            outputChannel->close();
                        }
                        else
                        {
                            OD_LOG("! (controlChannel && dataChannel && outputChannel && controlHandler && "//####
                                   "dataHandler)");//####
                            cerr << "Problem creating a port." << endl;
                        }
                        delete controlChannel;
                        delete dataChannel;
                        delete outputChannel;
                        
                        
                        
                        
#if 0
                        for ( ; lKeepRunning; )
                        {
                            char   inChar;
                            double newSum;
                            double value;
                            
                            cout << "Operation: [+ r x s]? ";
                            cin >> inChar;
                            switch (inChar)
                            {
                                case '+':
                                    cout << "add: ";
                                    cin >> value;
                                    cout << "adding " << value << endl;
                                    if (stuff->addToSum(value, newSum))
                                    {
                                        cout << "running sum = " << newSum << endl;
                                    }
                                    else
                                    {
                                        OD_LOG("! (stuff->addToSum(value, newSum))");//####
                                        cerr << "Problem adding to the sum." << endl;
                                    }
                                    break;
                                    
                                case 'r':
                                case 'R':
                                    cout << "Resetting" << endl;
                                    if (! stuff->resetSum())
                                    {
                                        OD_LOG("(! stuff->resetSum())");//####
                                        cerr << "Problem resetting the sum." << endl;
                                    }
                                    break;
                                    
                                case 's':
                                case 'S':
                                    cout << "Starting" << endl;
                                    if (! stuff->startSum())
                                    {
                                        OD_LOG("(! stuff->startSum())");//####
                                        cerr << "Problem starting the sum." << endl;
                                    }
                                    break;
                                    
                                case 'x':
                                case 'X':
                                    cout << "Exiting" << endl;
                                    if (! stuff->stopSum())
                                    {
                                        OD_LOG("(! stuff->stopSum())");//####
                                        cerr << "Problem stopping the sum." << endl;
                                    }
                                    lKeepRunning = false;
                                    break;
                                    
                                default:
                                    cout << "Unrecognized request '" << inChar << "'." << endl;
                                    break;
                                    
                            }
                        }
#endif//0
                        if (! stuff->disconnectFromService())
                        {
                            OD_LOG("(! stuff->disconnectFromService())");//####
                            cerr << "Problem discconnecting from the service." << endl;
                        }
                    }
                    else
                    {
                        OD_LOG("! (stuff->connectToService())");//####
                        cerr << "Problem connecting to the service." << endl;
                    }
                }
                else
                {
                    OD_LOG("! (stuff->findService(\"Name RunningSum\"))");//####
                    cerr << "Problem locating the service." << endl;
                }
                delete stuff;
            }
            else
            {
                OD_LOG("! (stuff)");//####
            }
        }
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork())");//####
            cerr << "YARP network not running." << endl;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0);//####
    return 0;
} // main
