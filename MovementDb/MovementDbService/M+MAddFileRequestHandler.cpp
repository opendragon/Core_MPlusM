//--------------------------------------------------------------------------------------------------
//
//  File:       M+MAddFileRequestHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the request handler for a 'reset' request.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and/or
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
//  Created:    2014-09-02
//
//--------------------------------------------------------------------------------------------------

#include "M+MAddFileRequestHandler.h"
#include "M+MMovementDbRequests.h"
#include "M+MMovementDbService.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the request handler for a 'reset' request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::MovementDb;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'reset' request. */
#define ADDFILE_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

AddFileRequestHandler::AddFileRequestHandler(MovementDbService & service) :
    inherited(MpM_ADDFILE_REQUEST), _service(service)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("service = ", &service); //####
    OD_LOG_EXIT_P(this); //####
} // AddFileRequestHandler::AddFileRequestHandler

AddFileRequestHandler::~AddFileRequestHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // AddFileRequestHandler::~AddFileRequestHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void AddFileRequestHandler::fillInAliases(Common::StringVector & alternateNames)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(alternateNames)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("alternateNames = ", &alternateNames); //####
    OD_LOG_OBJEXIT(); //####
} // AddFileRequestHandler::fillInAliases

void AddFileRequestHandler::fillInDescription(const yarp::os::ConstString & request,
                                              yarp::os::Property &          info)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("request = ", request); //####
    OD_LOG_P1("info = ", &info); //####
    try
    {
        info.put(MpM_REQREP_DICT_REQUEST_KEY, request);
        info.put(MpM_REQREP_DICT_VERSION_KEY, ADDFILE_REQUEST_VERSION_NUMBER);
        info.put(MpM_REQREP_DICT_DETAILS_KEY, "Add a file to the backend database\n"
                 "Input: nothing\n"
                 "Output: nothing");
        yarp::os::Value    keywords;
        yarp::os::Bottle * asList = keywords.asList();
        
        asList->addString(request);
        info.put(MpM_REQREP_DICT_KEYWORDS_KEY, keywords);
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // AddFileRequestHandler::fillInDescription

bool AddFileRequestHandler::processRequest(const yarp::os::ConstString & request,
                                           const yarp::os::Bottle &      restOfInput,
                                           const yarp::os::ConstString & senderChannel,
                                           yarp::os::ConnectionWriter *  replyMechanism)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(request,restOfInput)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER(); //####
    OD_LOG_S3s("request = ", request, "restOfInput = ", restOfInput.toString(), //####
               "senderChannel = ", senderChannel); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    bool result = true;
    
    try
    {
        yarp::os::Bottle reply;

        // Add the file to the backend database
        if (3 == restOfInput.size())
        {
            yarp::os::Value firstValue(restOfInput.get(0));
            yarp::os::Value secondValue(restOfInput.get(1));
            yarp::os::Value thirdValue(restOfInput.get(2));
            
            if (firstValue.isString() && secondValue.isString() && thirdValue.isString())
            {
                yarp::os::ConstString emailAddress(firstValue.toString());
                yarp::os::ConstString dataTrack(secondValue.toString());
                yarp::os::ConstString filePath(thirdValue.toString());

                if (_service.addFileToDb(emailAddress, dataTrack, filePath))
                {
                    reply.addString(MpM_OK_RESPONSE);
                }
                else
                {
                    OD_LOG("! (_service.addFileToDb(emailAddress, dataTrack, filePath))"); //####
                    reply.addString(MpM_FAILED_RESPONSE);
                    reply.addString("Could not add file to database");
                }
            }
            else
            {
                OD_LOG("! (firstValue.isString() && secondValue.isString() && " //####
                       "thirdValue.isString())"); //####
                reply.addString(MpM_FAILED_RESPONSE);
                reply.addString("Invalid arguments");
            }
        }
        else
        {
            OD_LOG("! (3 == restOfInput.size())"); //####
            reply.addString(MpM_FAILED_RESPONSE);
            reply.addString("Missing or extra arguments to request");
        }
        if (replyMechanism)
        {
            OD_LOG("(replyMechanism)"); //####
            OD_LOG_S1s("response <- ", reply.toString()); //####
            if (! reply.write(*replyMechanism))
            {
                OD_LOG("(! reply(*replyMechanism))"); //####
#if defined(MpM_StallOnSendProblem)
                Common::Stall();
#endif // defined(MpM_StallOnSendProblem)
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // AddFileRequestHandler::processRequest

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)