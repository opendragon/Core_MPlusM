//--------------------------------------------------------------------------------------------------
//
//  File:       M+MMovementDbClient.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the client of the movement database service.
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

#include "M+MMovementDbClient.h"
#include "M+MMovementDbRequests.h"

#include <mpm/M+MServiceResponse.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the client of the movement database service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::MovementDb;

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
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

MovementDbClient::MovementDbClient(void) :
    inherited("movementdb_")
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // MovementDbClient::MovementDbClient

MovementDbClient::~MovementDbClient(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // MovementDbClient::~MovementDbClient

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

/*! @brief Add a file entry to the backend database.
 @param filePath The filesystem path to the file.
 @returns @c true if the file entry was added successfully and @c false otherwise. */
bool MovementDbClient::addFileToDb(const yarp::os::ConstString & filePath)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("filePath = ", filePath); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        ServiceResponse  response;
        
        reconnectIfDisconnected(NULL, NULL);
        parameters.addString(filePath);
        if (send(MpM_ADDFILE_REQUEST, parameters, &response))
        {
            // Check that we got a successful add!
            if (1 == response.count())
            {
                yarp::os::Value theValue = response.element(0);
                
                if (theValue.isString())
                {
                    okSoFar = (theValue.toString() == MpM_OK_RESPONSE);
                }
                else
                {
                    OD_LOG("! (theValue.isString())"); //####
                }
            }
            else
            {
                OD_LOG("! (1 == response.count())"); //####
                OD_LOG_S1s("response = ", response.asString()); //####
            }
        }
        else
        {
            OD_LOG("! (send(MpM_ADDFILE_REQUEST, parameters, &response))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // MovementDbClient::addFileToDb

bool MovementDbClient::setDataTrackForDb(const yarp::os::ConstString & dataTrack)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("dataTrack = ", dataTrack); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        ServiceResponse  response;
        
        reconnectIfDisconnected(NULL, NULL);
        parameters.addString(dataTrack);
        if (send(MpM_SETDATATRACK_REQUEST, parameters, &response))
        {
            // Check that we got a successful set!
            if (1 == response.count())
            {
                yarp::os::Value theValue = response.element(0);
                
                if (theValue.isString())
                {
                    okSoFar = (theValue.toString() == MpM_OK_RESPONSE);
                }
                else
                {
                    OD_LOG("! (theValue.isString())"); //####
                }
            }
            else
            {
                OD_LOG("! (1 == response.count())"); //####
                OD_LOG_S1s("response = ", response.asString()); //####
            }
        }
        else
        {
            OD_LOG("! (send(MpM_SETDATATRACK_REQUEST, parameters, &response))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // MovementDbClient::setDataTrackForDb

bool MovementDbClient::setEmailAddressForDb(const yarp::os::ConstString & emailAddress)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("emailAddress = ", emailAddress); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        ServiceResponse  response;
        
        reconnectIfDisconnected(NULL, NULL);
        parameters.addString(emailAddress);
        if (send(MpM_SETEMAIL_REQUEST, parameters, &response))
        {
            // Check that we got a successful set!
            if (1 == response.count())
            {
                yarp::os::Value theValue = response.element(0);
                
                if (theValue.isString())
                {
                    okSoFar = (theValue.toString() == MpM_OK_RESPONSE);
                }
                else
                {
                    OD_LOG("! (theValue.isString())"); //####
                }
            }
            else
            {
                OD_LOG("! (1 == response.count())"); //####
                OD_LOG_S1s("response = ", response.asString()); //####
            }
        }
        else
        {
            OD_LOG("! (send(MpM_SETEMAIL_REQUEST, parameters, &response))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // MovementDbClient::setEmailAddressForDb

bool MovementDbClient::stopDbConnection(void)
{
    OD_LOG_OBJENTER(); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        
        reconnectIfDisconnected(NULL, NULL);
        if (send(MpM_STOPDB_REQUEST, parameters))
        {
            okSoFar = true;
        }
        else
        {
            OD_LOG("! (send(MpM_STOPDB_REQUEST, parameters))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);
    return okSoFar;
} // MovementDbClient::stopDbConnection


#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
