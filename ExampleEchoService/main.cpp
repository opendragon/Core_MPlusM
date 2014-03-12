//
//  ExampleEchoService/main.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPEndpoint.h"
#include "YPPExampleEchoService.h"
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

    if (argc > 1)
    {
        serviceEndpointName = argv[1];
        if (argc > 2)
        {
            serviceHostName = argv[2];
            if (argc > 3)
            {
                servicePortNumber = argv[3];
            }
        }
    }
    else
    {
        serviceEndpointName = DEFAULT_ECHO_SERVICE_NAME;
    }
    ExampleEchoService * stuff = new ExampleEchoService(serviceEndpointName, serviceHostName, servicePortNumber);
    
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

