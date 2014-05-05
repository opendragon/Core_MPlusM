//--------------------------------------------------------------------------------------
//
//  File:       M+MInputHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the interface between M+M input handlers and
//              YARP.
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
//  Created:    2014-02-11
//
//--------------------------------------------------------------------------------------

#include "M+MInputHandler.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the interface between M+M input handlers and YARP. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

//#define REPORT_CONTACT_DETAILS /* Report details of the contacts during operations that might change them. */

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

InputHandler::InputHandler(void) :
        inherited(), _canProcessInput(true)
{
    OD_LOG_ENTER();//####
    OD_LOG_EXIT_P(this);//####
} // InputHandler::InputHandler

InputHandler::~InputHandler(void)
{
    OD_LOG_OBJENTER();//####
    stopProcessing();
    OD_LOG_OBJEXIT();//####
} // InputHandler::~InputHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool InputHandler::read(yarp::os::ConnectionReader & connection)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("connection = ", &connection);//####
    bool result = true;
    
    try
    {
        if (_canProcessInput)
        {
#if defined(REPORT_CONTACT_DETAILS)
            DumpContact("input read", connection.getRemoteContact());//####
#endif // defined(REPORT_CONTACT_DETAILS)
            Package aPackage;
            
            if (aPackage.read(connection))
            {
                result = handleInput(aPackage, connection.getRemoteContact().getName(), connection.getWriter());
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // InputHandler::read

void InputHandler::stopProcessing(void)
{
    OD_LOG_OBJENTER();//####
    _canProcessInput = false;
    OD_LOG_OBJEXIT();//####
} // InputHandler::stopProcessing

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
