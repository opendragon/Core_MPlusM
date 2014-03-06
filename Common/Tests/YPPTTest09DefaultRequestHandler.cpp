//
//  YPPTTest09DefaultRequestHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest09DefaultRequestHandler.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"

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

Test09DefaultRequestHandler::Test09DefaultRequestHandler(void) :
        inherited("")
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // Test09DefaultRequestHandler::Test09DefaultRequestHandler

Test09DefaultRequestHandler::~Test09DefaultRequestHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test09DefaultRequestHandler::~Test09DefaultRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void Test09DefaultRequestHandler::fillInDescription(yarp::os::Property & info)
{
#pragma unused(info)
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test09DefaultRequestHandler::fillInDescription

bool Test09DefaultRequestHandler::operator() (const yarp::os::Bottle &     restOfInput,
                                              yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("restOfInput = ", restOfInput.toString().c_str());//####
    bool result = true;
    
    if (replyMechanism)
    {
        yarp::os::Bottle argsCopy(name());
        
        argsCopy.append(restOfInput);
        argsCopy.write(*replyMechanism);
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Test09DefaultRequestHandler::operator()

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
