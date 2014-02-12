//
//  RegistryService/main.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include <yarp/os/all.h>
#include <yarp/conf/version.h>
#include <iostream>

#include "YPPRegistryService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPConfig.h"

using namespace yarp::os;
using namespace YarpPlusPlus;
using std::cout;
using std::cerr;
using std::endl;

int main(int     argc,
         char ** argv)
{
#pragma unused(argc,argv)
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID);//####
    OD_SYSLOG_ENTER();//####
    Network yarp; // This is necessary to establish any connection to the YARP infrastructure
    
    cout << "YARP++ Version " << YPP_VERSION << ", YARP Version " << YARP_VERSION_STRING << endl;
    RegistryService * stuff = new RegistryService;

    delete stuff;
    OD_SYSLOG_EXIT_L(0);//####
    return 0;
} // main
