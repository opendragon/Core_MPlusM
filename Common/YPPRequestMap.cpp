//--------------------------------------------------------------------------------------
//
//  File:       YPPRequestMap.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for the mapping between requests and request handlers.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-02-28
//
//--------------------------------------------------------------------------------------

#include "YPPRequestMap.h"
//#include "ODEnableLogging.h"
#include "ODLogging.h"
#include "YPPBaseRequestHandler.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the mapping between requests and request handlers. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

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
    OD_LOG_ENTER();//####
    OD_LOG_P1("owner = ", &owner);//####
    OD_LOG_EXIT_P(this);//####
} // RequestMap::RequestMap

RequestMap::~RequestMap(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // RequestMap::~RequestMap

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void RequestMap::fillInListReply(Package & reply)
{
    OD_LOG_OBJENTER();//####
    try
    {
        RequestHandlerMap::const_iterator walker(_handlers.cbegin());
        
        for ( ; walker != _handlers.cend(); ++walker)
        {
            yarp::os::Property & aDict = reply.addDict();
            BaseRequestHandler * aHandler = walker->second;
            
            aHandler->fillInDescription(walker->first.c_str(), aDict);
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RequestMap::fillInListReply

void RequestMap::fillInRequestInfo(Package &                     reply,
                                   const yarp::os::ConstString & requestName)
{
    OD_LOG_OBJENTER();//####
    try
    {
        RequestHandlerMap::const_iterator match(_handlers.find(std::string(requestName)));
        
        if (_handlers.cend() == match)
        {
            OD_LOG("(_handlers.cend() == match)");//####
        }
        else
        {
            yarp::os::Property & aDict = reply.addDict();
            BaseRequestHandler * aHandler = match->second;
            
            aHandler->fillInDescription(match->first.c_str(), aDict);
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RequestMap::fillInRequestInfo

BaseRequestHandler * RequestMap::lookupRequestHandler(const yarp::os::ConstString & request)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("request = ", request.c_str());//####
    BaseRequestHandler * result = NULL;
    
    try
    {
        RequestHandlerMap::const_iterator match(_handlers.find(std::string(request)));

        if (_handlers.cend() == match)
        {
            OD_LOG("(_handlers.cend() == match)");//####
            result = _defaultHandler;
        }
        else
        {
            OD_LOG("! (_handlers.cend() == match)");//####
            result = match->second;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_P(result);//####
    return result;
} // RequestMap::lookupRequestHandler

void RequestMap::registerRequestHandler(BaseRequestHandler * handler)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("handler = ", handler);//####
    try
    {
        if (handler)
        {
            StringVector aliases;
            
            handler->fillInAliases(aliases);
            _handlers.insert(RequestHandlerMapValue(std::string(handler->name()), handler));
            for (StringVector::const_iterator it(aliases.cbegin()); it != aliases.cend(); ++it)
            {
                _handlers.insert(RequestHandlerMapValue(*it, handler));
            }
            handler->setOwner(*this);
        }
        OD_LOG_LL1("_handlers.size = ", _handlers.size());//####
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RequestMap::registerRequestHandler

void RequestMap::setDefaultRequestHandler(BaseRequestHandler * handler)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("handler = ", handler);//####
    _defaultHandler = handler;
    OD_LOG_OBJEXIT();//####
} // RequestMap::setDefaultRequestHandler

void RequestMap::unregisterRequestHandler(BaseRequestHandler * handler)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("handler = ", handler);//####
    try
    {
        if (handler)
        {
            StringVector aliases;
            
            handler->fillInAliases(aliases);
            _handlers.erase(std::string(handler->name()));
            for (StringVector::const_iterator it(aliases.cbegin()); it != aliases.cend(); ++it)
            {
                _handlers.erase(*it);
            }
        }
        OD_LOG_LL1("_handlers.size = ", _handlers.size());//####
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RequestMap::unregisterRequestHandler

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
