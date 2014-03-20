//--------------------------------------------------------------------------------------
//
//  File:       ExampleRandomNumberClient/main.cpp
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
//  Created:    2014-03-06
//
//--------------------------------------------------------------------------------------

//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPExampleRandomNumberClient.h"
#include <iostream>
#include <yarp/os/all.h>

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
#if defined(ENABLE_OD_SYSLOG)
# pragma unused(argc)
#else // ! defined(ENABLE_OD_SYSLOG)
# pragma unused(argc,argv)
#endif // ! defined(ENABLE_OD_SYSLOG)
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID |//####
                   kODSyslogOptionEnableThreadSupport | kODSyslogOptionWriteToStderr);//####
    OD_SYSLOG_ENTER();//####
    try
    {
        if (yarp::os::Network::checkNetwork())
        {
#if defined(ENABLE_OD_SYSLOG)
            yarp::os::Network::setVerbosity(1);
#else // ! defined(ENABLE_OD_SYSLOG)
            yarp::os::Network::setVerbosity(-1);
#endif // ! defined(ENABLE_OD_SYSLOG)
            yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
            
            YarpPlusPlus::Initialize();
            ExampleRandomNumberClient * stuff = new ExampleRandomNumberClient;
            
            if (stuff)
            {
                lKeepRunning = true;
#if (defined(__APPLE__) || defined(__linux__))
                signal(SIGHUP, stopRunning);
                signal(SIGINT, stopRunning);
                signal(SIGINT, stopRunning);
                signal(SIGUSR1, stopRunning);
#endif // defined(__APPLE__) || defined(__linux__)
                if (stuff->findService("keyword random"))
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
                                OD_SYSLOG("! (stuff->getOneRandomNumber(result))");//####
                                cerr << "Problem getting random number from service." << endl;
                            }
                        }
                        else
                        {
                            ExampleRandomNumberClient::RandomVector results;
                            
                            if (stuff->getRandomNumbers(count, results))
                            {
                                cout << "result = ( ";
                                for (ExampleRandomNumberClient::RandomVectorIterator it(results.begin());
                                     it != results.end(); ++it)
                                {
                                    cout << " " << *it;
                                }
                                cout << " )" << endl;
                            }
                            else
                            {
                                OD_SYSLOG("! (stuff->getRandomNumbers(count, results))");//####
                                cerr << "Problem getting random numbers from service." << endl;
                            }
                        }
                    }
                }
                else
                {
                    OD_SYSLOG("! (stuff->findService(\"keyword random\"))");//####
                    cerr << "Problem finding the service." << endl;
                }
                delete stuff;
            }
            else
            {
                OD_SYSLOG("! (stuff)");//####
            }
        }
        else
        {
            OD_SYSLOG("! (yarp::os::Network::checkNetwork())");//####
            cerr << "YARP network not running." << endl;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
    }
    yarp::os::Network::fini();
    OD_SYSLOG_EXIT_L(0);//####
    return 0;
} // main
