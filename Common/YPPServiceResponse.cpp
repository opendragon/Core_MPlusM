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

YarpPlusPlus::ServiceResponse::ServiceResponse(const ParameterType * values)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("values = ", values);//####
    if (values)
    {
        OD_SYSLOG_LL1("values size = ", values->size());//####
        for (size_t ii = 0; ii < values->size(); ++ii)
        {
            OD_SYSLOG_S1("value = ", values->at(ii).c_str());//####
        }
        _values = *values;
    }
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ServiceResponse::ServiceResponse

YarpPlusPlus::ServiceResponse::ServiceResponse(const yarp::os::Bottle & values)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_LL1("input size = ", values.size());//####
    for (int ii = 0; ii < values.size(); ++ii)
    {
        yarp::os::Value & element = values.get(ii);
        
        _values.push_back(element.asString());
    }
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
    _values.clear();
    for (int ii = 0; ii < values.size(); ++ii)
    {
        yarp::os::Value & element = values.get(ii);
        
        _values.push_back(element.asString());
    }
    OD_SYSLOG_EXIT_P(this);//####
    return *this;
} // YarpPlusPlus::ServiceResponse::operator=

size_t YarpPlusPlus::ServiceResponse::count(void)
const
{
    return _values.size();
} // YarpPlusPlus::ServiceResponse::count

yarp::os::ConstString YarpPlusPlus::ServiceResponse::element(const size_t index)
const
{
    OD_SYSLOG_ENTER();//####
    yarp::os::ConstString result;
    
    if (index < _values.size())
    {
        result = _values.at(index);
    }
    OD_SYSLOG_EXIT_S(result.c_str());//####
    return result;
} // YarpPlusPlus::ServiceResponse::element

#pragma mark Accessors

#pragma mark Global functions
