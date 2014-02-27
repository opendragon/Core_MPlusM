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
#include <yarp/os/all.h>
#include <yarp/conf/version.h>
#include <iostream>
#include <unistd.h>
#include <string.h>

using namespace YarpPlusPlus;
using std::cout;
using std::cerr;
using std::endl;

#pragma mark Private structures and constants

/*! @brief Run loop control; @c true if the service is to keep going and @c false otherwise. */
static bool lKeepRunning;

#pragma mark Local functions

/*! @brief The signal handler to catch requests to stop the service.
 @param signal The signal being handled. */
static void stopRunning(int signal)
{
#pragma unused(signal)
    lKeepRunning = false;
} // stopRunning

#pragma mark Global functions

/*! @brief The entry point for creating the Service Registry service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Service Registry service.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID |//####
                   kODSyslogOptionEnableThreadSupport);//####
    OD_SYSLOG_ENTER();//####
    yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
    
    cout << "YARP++ Version " << YPP_VERSION << ", YARP Version " << YARP_VERSION_STRING << endl;
    // Note that we need to copy any arguments, preceding them with our standard port name.
    char ** argvActual = (char **) malloc(static_cast<size_t>(argc + 1) * sizeof(char *));
    
    *argvActual = strdup(YPP_SERVICE_REGISTRY_PORT_NAME);
    for (int ii = 1; ii < argc; ++ii)
    {
        argvActual[ii] = argv[ii];
    }
    RegistryService * stuff = new RegistryService(argc, argvActual);
    
    if (stuff)
    {
        if (stuff->start())
        {
            lKeepRunning = true;
            signal(SIGHUP, stopRunning);
            signal(SIGINT, stopRunning);
            signal(SIGINT, stopRunning);
            signal(SIGUSR1, stopRunning);
            for ( ; lKeepRunning; )
            {
                sleep(1);
            }
            stuff->stop();
        }
        delete stuff;
    }
    free(*argvActual);
    free(argvActual);
    OD_SYSLOG_EXIT_L(0);//####
    return 0;
} // main
