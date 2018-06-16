//--------------------------------------------------------------------------------------------------
//
//  File:       m+mJavaScriptFilterInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the input channel input handler used by the JavaScript
//              filter service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2015-01-05
//
//--------------------------------------------------------------------------------------------------

#include "m+mJavaScriptFilterInputHandler.hpp"
#include "m+mJavaScriptFilterService.hpp"

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the input channel input handler used by the %JavaScript filter
 service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::JavaScript;
using std::cerr;
using std::endl;

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

JavaScriptFilterInputHandler::JavaScriptFilterInputHandler(JavaScriptFilterService * owner,
                                                           const size_t              slotNumber) :
    inherited(), _owner(owner), _slotNumber(slotNumber), _active(false)
{
    ODL_ENTER(); //####
    ODL_P1("owner = ", owner); //####
    ODL_L1("slotNumber = ", slotNumber); //####
    ODL_EXIT_P(this); //####
} // JavaScriptFilterInputHandler::JavaScriptFilterInputHandler

JavaScriptFilterInputHandler::~JavaScriptFilterInputHandler(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // JavaScriptFilterInputHandler::~JavaScriptFilterInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! defined(MAC_OR_LINUX_))
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! defined(MAC_OR_LINUX_)
bool
JavaScriptFilterInputHandler::handleInput(const yarp::os::Bottle &     input,
                                          const YarpString &           senderChannel,
                                          yarp::os::ConnectionWriter * replyMechanism,
                                          const size_t                 numBytes)
{
#if (! defined(ODL_ENABLE_LOGGING_))
# if defined(MAC_OR_LINUX_)
#  pragma unused(senderChannel,replyMechanism)
# endif // defined(MAC_OR_LINUX_)
#endif // ! defined(ODL_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_S2s("senderChannel = ", senderChannel, "got ", input.toString()); //####
    ODL_P1("replyMechanism = ", replyMechanism); //####
    ODL_L1("numBytes = ", numBytes); //####
    bool result = true;

    try
    {
        if (_active && _owner)
        {
            _received = input;
            _owner->stallUntilIdle(_slotNumber);
            _owner->signalRunFunction();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // JavaScriptFilterInputHandler::handleInput
#if (! defined(MAC_OR_LINUX_))
# pragma warning(pop)
#endif // ! defined(MAC_OR_LINUX_)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
