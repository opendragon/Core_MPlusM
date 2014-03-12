//
//  YPPRequestMap.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPRequestMap.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPRequestHandler.h"

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
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

RequestMap::RequestMap(BaseService & owner) :
        _defaultHandler(NULL), _handlers(), _owner(owner)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("owner = ", &owner);//####
    OD_SYSLOG_EXIT_P(this);//####
} // RequestMap::RequestMap

RequestMap::~RequestMap(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // RequestMap::~RequestMap

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void RequestMap::fillInListReply(yarp::os::Bottle & reply)
{
    OD_SYSLOG_ENTER();//####
    RequestHandlerMap::const_iterator walker(_handlers.cbegin());
    
    for ( ; walker != _handlers.cend(); ++walker)
    {
        yarp::os::Property & aDict = reply.addDict();
        RequestHandler *     aHandler = walker->second;
        
        aHandler->fillInDescription(aDict);
    }
    OD_SYSLOG_EXIT();//####
} // RequestMap::fillInListReply

void RequestMap::fillInRequestInfo(yarp::os::Bottle &            reply,
                                   const yarp::os::ConstString & requestName)
{
    OD_SYSLOG_ENTER();//####
    RequestHandlerMap::const_iterator match(_handlers.find(std::string(requestName)));
    
    if (_handlers.cend() != match)
    {
        yarp::os::Property & aDict = reply.addDict();
        RequestHandler *     aHandler = match->second;
        
        aHandler->fillInDescription(aDict);
    }
    OD_SYSLOG_EXIT();//####
} // RequestMap::fillInRequestInfo

RequestHandler * RequestMap::lookupRequestHandler(const yarp::os::ConstString & request)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("request = ", request.c_str());//####
    RequestHandlerMap::const_iterator match(_handlers.find(std::string(request)));
    RequestHandler *                  result;
    
    if (_handlers.cend() == match)
    {
        OD_SYSLOG("(_handlers.cend() == match)");//####
        result = _defaultHandler;
    }
    else
    {
        OD_SYSLOG("! (_handlers.cend() == match)");//####
        result = match->second;
    }
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // RequestMap::lookupRequestHandler

void RequestMap::registerRequestHandler(RequestHandler * handler)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("handler = ", handler);//####

    if (handler)
    {
        _handlers.insert(RequestHandlerMapValue(std::string(handler->name()), handler));
        handler->setOwner(*this);
    }
    OD_SYSLOG_LL1("_handlers.size = ", _handlers.size());//####
    OD_SYSLOG_EXIT();//####
} // RequestMap::registerRequestHandler

void RequestMap::setDefaultRequestHandler(RequestHandler * handler)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("handler = ", handler);//####
    _defaultHandler = handler;
    OD_SYSLOG_EXIT();//####
} // RequestMap::setDefaultRequestHandler

void RequestMap::unregisterRequestHandler(RequestHandler * handler)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("handler = ", handler);//####
    
    if (handler)
    {
        _handlers.erase(std::string(handler->name()));
    }
    OD_SYSLOG_LL1("_handlers.size = ", _handlers.size());//####
    OD_SYSLOG_EXIT();//####
} // RequestMap::unregisterRequestHandler

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
