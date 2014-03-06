//
//  YPPTTest16Service.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest16Service.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPTTest16EchoRequestHandler.h"
#include "../YPPRequests.h"

using namespace YarpPlusPlusTest;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

Test16Service::Test16Service(const int argc,
                             char **   argv) :
        inherited(true, argc, argv)
{
    OD_SYSLOG_ENTER();//####
    _requestHandlers.registerRequestHandler(new Test16EchoRequestHandler());
    OD_SYSLOG_EXIT_P(this);//####
} // Test16Service::Test16Service

Test16Service::~Test16Service(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test16Service::~Test16Service

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
