//
//  RegistryService/main.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPRegistryService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include <yarp/os/all.h>
#include <yarp/conf/version.h>
#include <iostream>

using namespace YarpPlusPlus;
using std::cout;
using std::cerr;
using std::endl;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Global functions

/*! @brief The entry point for creating the Service Registry service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Service Registry service.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
#if defined(ENABLE_OD_SYSLOG)
# pragma unused(argc)
#else // ! defined(ENABLE_OD_SYSLOG)
# pragma unused(argc,argv)
#endif // ! defined(ENABLE_OD_SYSLOG)
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID);//####
    OD_SYSLOG_ENTER();//####
    yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
    
    cout << "YARP++ Version " << YPP_VERSION << ", YARP Version " << YARP_VERSION_STRING << endl;
    RegistryService * stuff = new RegistryService;

    delete stuff;
    OD_SYSLOG_EXIT_L(0);//####
    return 0;
} // main
