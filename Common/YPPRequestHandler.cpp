//
//  YPPRequestHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-26.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPRequestHandler.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"

using namespace YarpPlusPlus;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

RequestHandler::RequestHandler(const yarp::os::ConstString & request,
                               BaseService &                 service) :
        _service(service), _name(request)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // RequestHandler::RequestHandler

RequestHandler::~RequestHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // RequestHandler::~RequestHandler

#pragma mark Actions

#pragma mark Accessors

#pragma mark Global functions
