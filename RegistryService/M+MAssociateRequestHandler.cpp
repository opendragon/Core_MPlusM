//--------------------------------------------------------------------------------------------------
//
//  File:       M+MAssociateRequestHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the request handler for the 'associate' request.
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
//  Created:    2014-05-22
//
//--------------------------------------------------------------------------------------------------

#include "M+MAssociateRequestHandler.h"
#include "M+MRegistryService.h"

#include <mpm/M+MEndpoint.h>
#include <mpm/M+MRequests.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the request handler for the 'associate' request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Registry;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'associate' request. */
#define ASSOCIATE_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

AssociateRequestHandler::AssociateRequestHandler(RegistryService & service) :
    inherited(MpM_ASSOCIATE_REQUEST, service)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("service = ", &service); //####
    OD_LOG_EXIT_P(this); //####
} // AssociateRequestHandler::AssociateRequestHandler

AssociateRequestHandler::~AssociateRequestHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // AssociateRequestHandler::~AssociateRequestHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void AssociateRequestHandler::fillInAliases(StringVector & alternateNames)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("alternateNames = ", &alternateNames); //####
    OD_LOG_OBJEXIT(); //####
} // AssociateRequestHandler::fillInAliases

void AssociateRequestHandler::fillInDescription(const yarp::os::ConstString & request,
                                                yarp::os::Property &          info)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("request = ", request); //####
    OD_LOG_P1("info = ", &info); //####
    try
    {
        info.put(MpM_REQREP_DICT_REQUEST_KEY, request);
        info.put(MpM_REQREP_DICT_INPUT_KEY, MpM_REQREP_STRING MpM_REQREP_INT MpM_REQREP_STRING);
        info.put(MpM_REQREP_DICT_OUTPUT_KEY, MpM_REQREP_STRING MpM_REQREP_STRING
                 MpM_REQREP_0_OR_MORE);
        info.put(MpM_REQREP_DICT_VERSION_KEY, ASSOCIATE_REQUEST_VERSION_NUMBER);
        info.put(MpM_REQREP_DICT_DETAILS_KEY, "Add an association between channels\n"
                 "Input: the primary channel, an integer (0 = input, 1 = output), the associated "
                 "channel\n"
                 "Output: OK or FAILED, with a description of the problem encountered");
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
} // AssociateRequestHandler::fillInDescription

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool AssociateRequestHandler::processRequest(const yarp::os::ConstString & request,
                                             const yarp::os::Bottle &      restOfInput,
                                             const yarp::os::ConstString & senderChannel,
                                             yarp::os::ConnectionWriter *  replyMechanism)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(request,senderChannel)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER(); //####
    OD_LOG_S3s("request = ", request, "restOfInput = ", restOfInput.toString(), //####
               "senderChannel = ", senderChannel); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    bool result = true;
    
    try
    {
        if (replyMechanism)
        {
            OD_LOG("(replyMechanism)"); //####
            yarp::os::Bottle reply;
            
            // Validate the name as a channel name
            if (3 == restOfInput.size())
            {
                yarp::os::Value primary(restOfInput.get(0));
                yarp::os::Value direction(restOfInput.get(1));
                yarp::os::Value secondary(restOfInput.get(2));
                
                if (primary.isString() && direction.isInt() && secondary.isString())
                {
                    yarp::os::ConstString primaryAsString(primary.toString());
                    yarp::os::ConstString secondaryAsString(secondary.toString());
                    
                    if (Endpoint::CheckEndpointName(primaryAsString) &&
                        Endpoint::CheckEndpointName(secondaryAsString))
                    {
                        RegistryService & theService = static_cast<RegistryService &>(_service);
                        
                        if (theService.checkForExistingAssociation(primaryAsString,
                                                                   secondaryAsString))
                        {
                            // This association is already known.
                            reply.addString(MpM_OK_RESPONSE);
                        }
                        else if (theService.addAssociation(primaryAsString, 0 != direction.asInt(),
                                                           secondaryAsString))
                        {
                            reply.addString(MpM_OK_RESPONSE);
                        }
                        else
                        {
                            OD_LOG("! (theService.addAssociation(primaryAsString, " //####
                                   "0 != direction, secondaryAsString))"); //####
                            reply.addString(MpM_FAILED_RESPONSE);
                            reply.addString("Could not add association");
                        }
                    }
                    else
                    {
                        OD_LOG("! (Endpoint::CheckEndpointName(primaryAsString) && " //####
                               "Endpoint::CheckEndpointName(secondaryAsString))"); //####
                        reply.addString(MpM_FAILED_RESPONSE);
                        reply.addString("Invalid channel name(s)");
                    }
                }
                else
                {
                    OD_LOG("! (primary.isString() && direction.isInt() && " //####
                           "secondary.isString())"); //####
                    reply.addString(MpM_FAILED_RESPONSE);
                    reply.addString("Invalid channel name(s)");
                }
            }
            else
            {
                OD_LOG("! (3 == restOfInput.size())"); //####
                reply.addString(MpM_FAILED_RESPONSE);
                reply.addString("Missing channel name(s) or extra arguments to request");
            }
            sendResponse(reply, replyMechanism);
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // AssociateRequestHandler::processRequest
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
