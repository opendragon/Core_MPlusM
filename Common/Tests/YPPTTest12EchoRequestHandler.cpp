//
//  YPPTTest12EchoRequestHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest12EchoRequestHandler.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "../YPPRequests.h"

using namespace YarpPlusPlusTest;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'echo' request. */
#define ECHO_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

Test12EchoRequestHandler::Test12EchoRequestHandler(void) :
        inherited(YPP_ECHO_REQUEST)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // Test12EchoRequestHandler::Test12EchoRequestHandler

Test12EchoRequestHandler::~Test12EchoRequestHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test12EchoRequestHandler::~Test12EchoRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void Test12EchoRequestHandler::fillInDescription(yarp::os::Property & info)
{
    OD_SYSLOG_ENTER();//####
    info.put(YPP_REQREP_DICT_REQUEST_KEY, YPP_ECHO_REQUEST);
    info.put(YPP_REQREP_DICT_INPUT_KEY, YPP_REQREP_ANYTHING YPP_REQREP_0_OR_MORE);
    info.put(YPP_REQREP_DICT_OUTPUT_KEY, YPP_REQREP_ANYTHING YPP_REQREP_0_OR_MORE);
    info.put(YPP_REQREP_DICT_VERSION_KEY, ECHO_REQUEST_VERSION_NUMBER);
    info.put(YPP_REQREP_DICT_DESCRIPTION_KEY, "Echo back any input");
    yarp::os::Value    keywords;
    yarp::os::Bottle * asList = keywords.asList();
    
    asList->addString(YPP_ECHO_REQUEST);
    info.put(YPP_REQREP_DICT_KEYWORDS_KEY, keywords);
    OD_SYSLOG_EXIT();//####
} // Test12EchoRequestHandler::fillInDescription

bool Test12EchoRequestHandler::operator() (const yarp::os::Bottle &     restOfInput,
                                           yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("restOfInput = ", restOfInput.toString().c_str());//####
    bool result = true;
    
    if (replyMechanism)
    {
        yarp::os::Bottle argsCopy(restOfInput);
        
        argsCopy.write(*replyMechanism);
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Test12EchoRequestHandler::operator()

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
