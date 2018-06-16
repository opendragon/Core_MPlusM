//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mSetMetricsStateRequestHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the request handler for the standard 'setMetricsState'
//              request.
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
//  Created:    2014-11-25
//
//--------------------------------------------------------------------------------------------------

#include "m+mSetMetricsStateRequestHandler.hpp"

#include <m+m/m+mBaseService.hpp>
#include <m+m/m+mRequests.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the request handler for the standard 'setMetricsState' request. */
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

/*! @brief The protocol version number for the 'setMetricsState' request. */
#define SETMETRICSSTATE_REQUEST_VERSION_NUMBER_ "1.0"

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

SetMetricsStateRequestHandler::SetMetricsStateRequestHandler(BaseService & service) :
    inherited(MpM_SETMETRICSSTATE_REQUEST_, service)
{
    ODL_ENTER(); //####
    ODL_P1("service = ", &service); //####
    ODL_EXIT_P(this); //####
} // SetMetricsStateRequestHandler::SetMetricsStateRequestHandler

SetMetricsStateRequestHandler::~SetMetricsStateRequestHandler(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // SetMetricsStateRequestHandler::~SetMetricsStateRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void
SetMetricsStateRequestHandler::fillInDescription(const YarpString &   request,
                                                 yarp::os::Property & info)
{
    ODL_OBJENTER(); //####
    ODL_S1s("request = ", request); //####
    ODL_P1("info = ", &info); //####
    try
    {
        info.put(MpM_REQREP_DICT_REQUEST_KEY_, request);
        info.put(MpM_REQREP_DICT_INPUT_KEY_, MpM_REQREP_INT_);
        info.put(MpM_REQREP_DICT_VERSION_KEY_, SETMETRICSSTATE_REQUEST_VERSION_NUMBER_);
        info.put(MpM_REQREP_DICT_DETAILS_KEY_, T_("Set the state of metrics collection for the "
                                                  "service\n"
                                                  "Input: 0 if metrics are to be disabled and 1 if "
                                                  "they are to be enabled\n"
                                                  "Output: nothing"));
        yarp::os::Value    keywords;
        yarp::os::Bottle * asList = keywords.asList();

        asList->addString(request);
        info.put(MpM_REQREP_DICT_KEYWORDS_KEY_, keywords);
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // SetMetricsStateRequestHandler::fillInDescription

#if (! defined(MAC_OR_LINUX_))
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! defined(MAC_OR_LINUX_)
bool
SetMetricsStateRequestHandler::processRequest(const YarpString &           request,
                                              const yarp::os::Bottle &     restOfInput,
                                              const YarpString &           senderChannel,
                                              yarp::os::ConnectionWriter * replyMechanism)
{
#if (! defined(ODL_ENABLE_LOGGING_))
# if defined(MAC_OR_LINUX_)
#  pragma unused(request,restOfInput,senderChannel)
# endif // defined(MAC_OR_LINUX_)
#endif // ! defined(ODL_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_S3s("request = ", request, "restOfInput = ", restOfInput.toString(), //####
            "senderChannel = ", senderChannel); //####
    ODL_P1("replyMechanism = ", replyMechanism); //####
    bool result = true;

    try
    {
        if (0 < restOfInput.size())
        {
            yarp::os::Value number(restOfInput.get(0));

            if (number.isInt())
            {
                int flag = number.asInt();

                if (flag)
                {
                    _service.enableMetrics();
                }
                else
                {
                    _service.disableMetrics();
                }
            }
        }
        sendOKResponse(replyMechanism);
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // SetMetricsStateRequestHandler::processRequest
#if (! defined(MAC_OR_LINUX_))
# pragma warning(pop)
#endif // ! defined(MAC_OR_LINUX_)

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
