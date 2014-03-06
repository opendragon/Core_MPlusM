//
//  YPPListRequestHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-26.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPListRequestHandler.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPRequestMap.h"
#include "YPPRequests.h"

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'list' request. */
#define LIST_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

ListRequestHandler::ListRequestHandler(void) :
        inherited(YPP_LIST_REQUEST)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // ListRequestHandler::ListRequestHandler

ListRequestHandler::~ListRequestHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // ListRequestHandler::~ListRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void ListRequestHandler::fillInDescription(yarp::os::Property & info)
{
    OD_SYSLOG_ENTER();//####
    info.put(YPP_REQREP_DICT_REQUEST_KEY, YPP_LIST_REQUEST);
    info.put(YPP_REQREP_DICT_OUTPUT_KEY, YPP_REQREP_LIST_START YPP_REQREP_DICT_START YPP_REQREP_DICT_END
             YPP_REQREP_1_OR_MORE YPP_REQREP_LIST_END);
    info.put(YPP_REQREP_DICT_VERSION_KEY, LIST_REQUEST_VERSION_NUMBER);
    info.put(YPP_REQREP_DICT_DESCRIPTION_KEY, "List the recognized requests");
    yarp::os::Value    keywords;
    yarp::os::Bottle * asList = keywords.asList();
    
    asList->addString(YPP_LIST_REQUEST);
    asList->addString("requests");
    asList->addString("methods");
    asList->addString("operations");
#if 0
    asList->addString("key-" YPP_REQREP_DICT_REQUEST_KEY);
    asList->addString("key-" YPP_REQREP_DICT_OUTPUT_KEY);
    asList->addString("key-" YPP_REQREP_DICT_INPUT_KEY);
    asList->addString("key-" YPP_REQREP_DICT_VERSION_KEY);
    asList->addString("key-" YPP_REQREP_DICT_DESCRIPTION_KEY);
#endif//0
    info.put(YPP_REQREP_DICT_KEYWORDS_KEY, keywords);
    OD_SYSLOG_EXIT();//####
} // ListRequestHandler::fillInDescription

bool ListRequestHandler::operator() (const yarp::os::Bottle &     restOfInput,
                                     yarp::os::ConnectionWriter * replyMechanism)
{
#if (! defined(ENABLE_OD_SYSLOG))
# pragma unused(restOfInput)
#endif // ! defined(ENABLE_OD_SYSLOG)
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("restOfInput = ", restOfInput.toString().c_str());//####
    bool result = true;
    
    if (replyMechanism)
    {
        yarp::os::Bottle reply;
        
        if (_owner)
        {
            _owner->fillInListReply(reply);            
        }
        OD_SYSLOG_S1("reply <- ", reply.toString().c_str());
        reply.write(*replyMechanism);
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // ListRequestHandler::operator()

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
