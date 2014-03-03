//
//  YPPInfoRequestHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-27.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPInfoRequestHandler.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPBaseService.h"
#include "YPPRequests.h"

using namespace YarpPlusPlus;

#pragma mark Private structures and constants

/*! @brief The protocol version number for the 'info' request. */
#define INFO_REQUEST_VERSION_NUMBER "1.0"

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

InfoRequestHandler::InfoRequestHandler(void) :
        inherited(YPP_LIST_REQUEST)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // InfoRequestHandler::InfoRequestHandler

InfoRequestHandler::~InfoRequestHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // InfoRequestHandler::~InfoRequestHandler

#pragma mark Actions

void InfoRequestHandler::fillInDescription(yarp::os::Property & info)
{
    OD_SYSLOG_ENTER();//####
    info.put(YPP_REQREP_DICT_NAME_KEY, YPP_INFO_REQUEST);
    info.put(YPP_REQREP_DICT_INPUT_KEY, YPP_REQREP_ANYTHING YPP_REQREP_1_OR_MORE);
    info.put(YPP_REQREP_DICT_OUTPUT_KEY, YPP_REQREP_LIST_START YPP_REQREP_DICT_START YPP_REQREP_DICT_END
             YPP_REQREP_0_OR_1 YPP_REQREP_LIST_END);
    info.put(YPP_REQREP_DICT_VERSION_KEY, INFO_REQUEST_VERSION_NUMBER);
    info.put(YPP_REQREP_DICT_DESCRIPTION_KEY, "Return information on a request");
    yarp::os::Value    keywords;
    yarp::os::Bottle * asList = keywords.asList();
    
    asList->addString(YPP_INFO_REQUEST);
    asList->addString("key-" YPP_REQREP_DICT_NAME_KEY);
    asList->addString("key-" YPP_REQREP_DICT_OUTPUT_KEY);
    asList->addString("key-" YPP_REQREP_DICT_INPUT_KEY);
    asList->addString("key-" YPP_REQREP_DICT_VERSION_KEY);
    asList->addString("key-" YPP_REQREP_DICT_DESCRIPTION_KEY);
    info.put(YPP_REQREP_DICT_KEYWORDS_KEY, keywords);
    OD_SYSLOG_EXIT();//####
} // InfoRequestHandler::fillInDescription

bool InfoRequestHandler::operator() (const yarp::os::Bottle &     restOfInput,
                                     yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("restOfInput = ", restOfInput.toString().c_str());//####
    bool result = true;
    
    if (replyMechanism)
    {
        yarp::os::Bottle reply;
        
        if (_owner && (1 == restOfInput.size()))
        {
            _owner->fillInRequestInfo(reply, restOfInput.get(0).toString());
        }
        OD_SYSLOG_S1("reply <- ", reply.toString().c_str());
        reply.write(*replyMechanism);
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // InfoRequestHandler::operator()

#pragma mark Accessors

#pragma mark Global functions
