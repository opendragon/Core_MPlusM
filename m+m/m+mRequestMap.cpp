//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mRequestMap.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the mapping between requests and request handlers.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and / or
//                  other materials provided with the distribution.
//                * Neither the name of the copyright holders nor the names of its contributors may
//                  be used to endorse or promote products derived from this software without
//                  specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
//              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
//              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
//              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
//              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//              DAMAGE.
//
//  Created:    2014-02-28
//
//--------------------------------------------------------------------------------------------------

#include "m+mRequestMap.h"

#include <m+m/m+mBaseRequestHandler.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the mapping between requests and request handlers. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

RequestMap::RequestMap(BaseService & owner) :
    _defaultHandler(NULL), _handlers(), _owner(owner)
{
    ODL_ENTER(); //####
    ODL_P1("owner = ", &owner); //####
    ODL_EXIT_P(this); //####
} // RequestMap::RequestMap

RequestMap::~RequestMap(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // RequestMap::~RequestMap

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
RequestMap::fillInListReply(yarp::os::Bottle & reply)
{
    ODL_OBJENTER(); //####
    try
    {
        lock();
        if (0 < _handlers.size())
        {
            for (RequestHandlerMap::const_iterator walker = _handlers.begin();
                 _handlers.end() != walker; ++walker)
            {
                yarp::os::Property & aDict = reply.addDict();
                BaseRequestHandler * aHandler = walker->second;
                
                aHandler->fillInDescription(walker->first.c_str(), aDict);
            }
        }
        unlock();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RequestMap::fillInListReply

void
RequestMap::fillInRequestInfo(yarp::os::Bottle & reply,
                                   const YarpString & requestName)
{
    ODL_OBJENTER(); //####
    try
    {
        lock();
        RequestHandlerMap::const_iterator match(_handlers.find(requestName));
        
        if (_handlers.end() == match)
        {
            ODL_LOG("(_handlers.end() == match)"); //####
        }
        else
        {
            yarp::os::Property & aDict = reply.addDict();
            BaseRequestHandler * aHandler = match->second;
            
            aHandler->fillInDescription(match->first.c_str(), aDict);
        }
        unlock();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RequestMap::fillInRequestInfo

BaseRequestHandler *
RequestMap::lookupRequestHandler(const YarpString & request)
{
    ODL_OBJENTER(); //####
    ODL_S1s("request = ", request); //####
    BaseRequestHandler * result = NULL;
    
    try
    {
        lock();
        RequestHandlerMap::const_iterator match(_handlers.find(request));
        
        if (_handlers.end() == match)
        {
            ODL_LOG("(_handlers.end() == match)"); //####
            result = _defaultHandler;
        }
        else
        {
            ODL_LOG("! (_handlers.end() == match)"); //####
            result = match->second;
        }
        unlock();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_P(result); //####
    return result;
} // RequestMap::lookupRequestHandler

void
RequestMap::registerRequestHandler(BaseRequestHandler * handler)
{
    ODL_OBJENTER(); //####
    ODL_P1("handler = ", handler); //####
    try
    {
        if (handler)
        {
            YarpStringVector aliases;
            
            handler->fillInAliases(aliases);
            lock();
            _handlers.insert(RequestHandlerMapValue(handler->name(), handler));
            if (0 < aliases.size())
            {
                for (YarpStringVector::const_iterator walker(aliases.begin());
                     aliases.end() != walker; ++walker)
                {
                    _handlers.insert(RequestHandlerMapValue(*walker, handler));
                }
            }
            unlock();
            handler->setOwner(*this);
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RequestMap::registerRequestHandler

void
RequestMap::setDefaultRequestHandler(BaseRequestHandler * handler)
{
    ODL_OBJENTER(); //####
    ODL_P1("handler = ", handler); //####
    lock();
    _defaultHandler = handler;
    unlock();
    ODL_OBJEXIT(); //####
} // RequestMap::setDefaultRequestHandler

void
RequestMap::unregisterRequestHandler(BaseRequestHandler * handler)
{
    ODL_OBJENTER(); //####
    ODL_P1("handler = ", handler); //####
    try
    {
        if (handler)
        {
            YarpStringVector aliases;
            
            handler->fillInAliases(aliases);
            lock();
            _handlers.erase(handler->name());
            if (0 < aliases.size())
            {
                for (YarpStringVector::const_iterator walker(aliases.begin());
                     aliases.end() != walker; ++walker)
                {
                    _handlers.erase(*walker);
                }
            }
            unlock();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RequestMap::unregisterRequestHandler

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
