//--------------------------------------------------------------------------------------
//
//  File:       YPPBaseRequestHandler.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for the minimal functionality required for a Yarp++
//              request handler.
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
//  Created:    2014-02-26
//
//--------------------------------------------------------------------------------------

#include "YPPBaseRequestHandler.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"

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

BaseRequestHandler::BaseRequestHandler(const yarp::os::ConstString & request) :
        _owner(NULL), _name(request)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // BaseRequestHandler::BaseRequestHandler

BaseRequestHandler::~BaseRequestHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    OD_SYSLOG_EXIT();//####
} // BaseRequestHandler::~BaseRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void BaseRequestHandler::setOwner(RequestMap & owner)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    OD_SYSLOG_P1("owner = ", &owner);//####
    _owner = &owner;
    OD_SYSLOG_EXIT();//####
} // BaseRequestHandler::setOwner

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
