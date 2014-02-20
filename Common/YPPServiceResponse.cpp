//
//  YPPServiceResponse.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPServiceResponse.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

YarpPlusPlus::ServiceResponse::ServiceResponse(const yarp::os::Bottle & values) :
        _values(values)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_LL1("input size = ", values.size());//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ServiceResponse::ServiceResponse

YarpPlusPlus::ServiceResponse::~ServiceResponse(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ServiceResponse::~ServiceResponse

#pragma mark Actions

YarpPlusPlus::ServiceResponse & YarpPlusPlus::ServiceResponse::operator=(const yarp::os::Bottle & values)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_LL1("input size = ", values.size());//####
    _values = values;
    OD_SYSLOG_EXIT_P(this);//####
    return *this;
} // YarpPlusPlus::ServiceResponse::operator=

int YarpPlusPlus::ServiceResponse::count(void)
const
{
    return _values.size();
} // YarpPlusPlus::ServiceResponse::count

yarp::os::Value YarpPlusPlus::ServiceResponse::element(const int index)
const
{
    OD_SYSLOG_ENTER();//####
    yarp::os::Value result;
    
    if ((index >= 0) && (index < _values.size()))
    {
        result = _values.get(index);
    }
    OD_SYSLOG_EXIT_S(result.toString().c_str());//####
    return result;
} // YarpPlusPlus::ServiceResponse::element

#pragma mark Accessors

#pragma mark Global functions
