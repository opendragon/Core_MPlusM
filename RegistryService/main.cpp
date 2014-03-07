//
//  RegistryService/main.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPRegistryService.h"
#include "YPPRequests.h"
#include <ace/Version.h>
#include <iostream>
#include <string.h>
#if (defined(__APPLE__) || defined(__linux__))
# include <unistd.h>
#endif // defined(__APPLE__) || defined(__linux__)
#include <yarp/conf/version.h>
#include <yarp/os/all.h>

using namespace YarpPlusPlus;
using std::cout;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

/*! @brief Set to @c true to use an in-memory database and @c false to use a disk-based database. */
#define USE_INMEMORY true

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

/*! @brief The entry point for creating the Service Registry service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Service Registry service.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID |//####
                   kODSyslogOptionEnableThreadSupport);//####
    OD_SYSLOG_ENTER();//####
    yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
    RegistryService * stuff = NULL;
    
    cout << "YARP++ Version " << YPP_VERSION << ", YARP Version " << YARP_VERSION_STRING << ", ACE Version = " <<
            ACE_VERSION << endl;
    if (argc >= 1)
    {
        switch (argc)
        {
                // Argument order for tests = endpoint name [, IP address / name [, port]]
            case 1:
                stuff = new RegistryService(USE_INMEMORY);
                break;
                
            case 2:
                stuff = new RegistryService(USE_INMEMORY, argv[1]);
                break;
                
            case 3:
                stuff = new RegistryService(USE_INMEMORY, argv[1], argv[2]);
                break;
                
            default:
                break;
                
        }
    }
    if (stuff)
    {
        if (stuff->start())
        {
            // Note that the Registry Service is self-registering... so we don't need to call registerLocalService().
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
            stuff->stop();
        }
        delete stuff;
    }
    OD_SYSLOG_EXIT_L(0);//####
    return 0;
} // main
