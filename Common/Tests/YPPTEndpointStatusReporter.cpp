//
//  YPPTEndpointStatusReporter.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTEndpointStatusReporter.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include <yarp/os/PortInfo.h>

using namespace YarpPlusPlusTest;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

EndpointStatusReporter::EndpointStatusReporter(void) :
        inherited()
{
} // EndpointStatusReporter::EndpointStatusReporter

EndpointStatusReporter::~EndpointStatusReporter(void)
{
} // EndpointStatusReporter::~EndpointStatusReporter

#pragma mark Actions

void EndpointStatusReporter::report(const yarp::os::PortInfo & info)
{
    OD_SYSLOG_LL1("tag = ", info.tag);
    OD_SYSLOG_B2("incoming = ", info.incoming, "created = ", info.created);
    OD_SYSLOG_S4("portName = ", info.portName.c_str(), "sourceName = ", info.sourceName.c_str(),
                 "targetName = ", info.targetName.c_str(), "carrierName = ", info.carrierName.c_str());
    OD_SYSLOG_S1("message = ", info.message.c_str());
} // EndpointStatusReporter::report

#pragma mark Accessors

#pragma mark Global functions
