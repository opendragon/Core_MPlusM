//--------------------------------------------------------------------------------------
//
//  File:       ExampleRandomNumberService/main.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The main application for a simple Yarp++ service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by OpenDragon.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------

//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPEndpoint.h"
#include "YPPExampleRandomNumberService.h"
#include "YPPRegistryService.h"
#include <ace/Version.h>
#include <iostream>
#if (defined(__APPLE__) || defined(__linux__))
# include <unistd.h>
#endif // defined(__APPLE__) || defined(__linux__)
#include <yarp/conf/version.h>
#include <yarp/os/all.h>

using namespace YarpPlusPlusExample;
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

/*! @brief The entry point for creating an example service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example service.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID |//####
                   kODSyslogOptionEnableThreadSupport);//####
    OD_SYSLOG_ENTER();//####
    cout << "YARP++ Version " << YPP_VERSION << ", YARP Version " << YARP_VERSION_STRING << ", ACE Version = " <<
            ACE_VERSION << endl;
    yarp::os::ConstString serviceEndpointName;
    yarp::os::ConstString serviceHostName;
    yarp::os::ConstString servicePortNumber;
    yarp::os::Network     yarp; // This is necessary to establish any connection to the YARP infrastructure

    if (1 < argc)
    {
        serviceEndpointName = argv[1];
        if (2 < argc)
        {
            serviceHostName = argv[2];
            if (3 < argc)
            {
                servicePortNumber = argv[3];
            }
        }
    }
    else
    {
        serviceEndpointName = DEFAULT_RANDOM_SERVICE_NAME;
    }
    ExampleRandomNumberService * stuff = new ExampleRandomNumberService(serviceEndpointName, serviceHostName,
                                                                        servicePortNumber);
    
    if (stuff)
    {
        if (stuff->start())
        {
            yarp::os::ConstString portName(stuff->getEndpoint().getName());
            
            OD_SYSLOG_S1("portName = ", portName.c_str());//####
            if (YarpPlusPlus::RegisterLocalService(portName))
            {
                lKeepRunning = true;
#if (defined(__APPLE__) || defined(__linux__))
                signal(SIGHUP, stopRunning);
                signal(SIGINT, stopRunning);
                signal(SIGINT, stopRunning);
                signal(SIGUSR1, stopRunning);
#endif // defined(__APPLE__) || defined(__linux__)
                for ( ; lKeepRunning; )
                {
                    yarp::os::Time::delay(1.0);
                }
                YarpPlusPlus::UnregisterLocalService(portName);
                stuff->stop();
            }
        }
        delete stuff;
    }
    OD_SYSLOG_EXIT_L(0);//####
    return 0;
} // main

