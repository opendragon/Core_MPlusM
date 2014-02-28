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

RequestHandler::RequestHandler(const yarp::os::ConstString & request) :
        _mapper(NULL), _name(request)
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

void RequestHandler::setOwner(RequestMap & owner)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("owner = ", &owner);//####
    _mapper = &owner;
    OD_SYSLOG_EXIT();//####
} // RequestHandler::setOwner

#pragma mark Accessors

#pragma mark Global functions
