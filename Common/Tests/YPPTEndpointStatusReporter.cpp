//
//  YPPTEndpointStatusReporter.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTEndpointStatusReporter.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include <yarp/os/PortInfo.h>

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

EndpointStatusReporter::EndpointStatusReporter(void) :
        inherited()
{
} // EndpointStatusReporter::EndpointStatusReporter

EndpointStatusReporter::~EndpointStatusReporter(void)
{
} // EndpointStatusReporter::~EndpointStatusReporter

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void EndpointStatusReporter::report(const yarp::os::PortInfo & info)
{
#if (! defined(ENABLE_OD_SYSLOG))
# pragma unused(info)
#endif // ! defined(ENABLE_OD_SYSLOG)
    OD_SYSLOG_LL1("tag = ", info.tag);
    OD_SYSLOG_B2("incoming = ", info.incoming, "created = ", info.created);
    OD_SYSLOG_S4("portName = ", info.portName.c_str(), "sourceName = ", info.sourceName.c_str(),
                 "targetName = ", info.targetName.c_str(), "carrierName = ", info.carrierName.c_str());
    OD_SYSLOG_S1("message = ", info.message.c_str());
} // EndpointStatusReporter::report

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
