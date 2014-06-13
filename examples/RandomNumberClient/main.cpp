//--------------------------------------------------------------------------------------
//
//  File:       RandomNumberClient/main.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the client of a simple M+M service.
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
//  Created:    2014-03-06
//
//--------------------------------------------------------------------------------------

#include "M+MChannelStatusReporter.h"
#include "M+MRandomNumberClient.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

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

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The main application for the client of a simple M+M service. */

/*! @dir RandomNumberClient
 @brief The set of files that implement the client for a simple M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;
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

/*! @brief The signal handler to catch requests to stop the service.
 @param signal The signal being handled. */
static void stopRunning(int signal)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(signal)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_ENTER();//####
    OD_LOG_LL1("signal = ", signal);//####
    lKeepRunning = false;
    OD_LOG_EXIT();//####
} // stopRunning

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for communicating with the example Random Number service.
 
 Integers read from standard input will be sent to the service as the number of random numbers to generate. Entering a
 zero will exit the program.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
#if MAC_OR_LINUX_
# pragma unused(argc)
#endif // MAC_OR_LINUX_
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID |//####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr);//####
    OD_LOG_ENTER();//####
    try
    {
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork())
#endif // CheckNetworkWorks_
        {
            yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
            
            MplusM::Common::Initialize(*argv);
            RandomNumberClient * stuff = new RandomNumberClient;
            
            if (stuff)
            {
                lKeepRunning = true;
                MplusM::Common::SetSignalHandlers(stopRunning);
                if (stuff->findService("keyword random"))
                {
#if defined(MpM_ReportOnConnections)
                    MplusM::Common::ChannelStatusReporter reporter;
#endif // defined(MpM_ReportOnConnections)
                    
#if defined(MpM_ReportOnConnections)
                    stuff->setReporter(reporter, true);
#endif // defined(MpM_ReportOnConnections)
                    if (stuff->connectToService())
                    {
                        for ( ; lKeepRunning; )
                        {
                            int count;
                            
                            cout << "How many random numbers? ";
                            cin >> count;
                            if (0 >= count)
                            {
                                break;
                            }
                            
                            if (1 == count)
                            {
                                double result;
                                
                                if (stuff->getOneRandomNumber(result))
                                {
                                    cout << "result = " << result << endl;
                                }
                                else
                                {
                                    OD_LOG("! (stuff->getOneRandomNumber(result))");//####
                                    cerr << "Problem getting random number from service." << endl;
                                }
                            }
                            else
                            {
                                MplusM::Common::DoubleVector results;
                                
                                if (stuff->getRandomNumbers(count, results))
                                {
                                    cout << "result = ( ";
                                    for (MplusM::Common::DoubleVector::const_iterator it(results.begin());
                                         results.end() != it; ++it)
                                    {
                                        cout << " " << *it;
                                    }
                                    cout << " )" << endl;
                                }
                                else
                                {
                                    OD_LOG("! (stuff->getRandomNumbers(count, results))");//####
                                    cerr << "Problem getting random numbers from service." << endl;
                                }
                            }
                        }
                        if (! stuff->disconnectFromService())
                        {
                            OD_LOG("(! stuff->disconnectFromService())");//####
                            cerr << "Problem disconnecting from the service." << endl;
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
                    OD_LOG("! (stuff->findService(\"keyword random\"))");//####
                    cerr << "Problem finding the service." << endl;
                }
                delete stuff;
            }
            else
            {
                OD_LOG("! (stuff)");//####
            }
        }
#if CheckNetworkWorks_
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork())");//####
            cerr << "YARP network not running." << endl;
        }
#endif // CheckNetworkWorks_
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0);//####
    return 0;
} // main
