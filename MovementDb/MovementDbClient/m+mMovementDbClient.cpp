//--------------------------------------------------------------------------------------------------
//
//  File:       m+mMovementDbClient.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the client of the movement database service.
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
//  Created:    2014-09-02
//
//--------------------------------------------------------------------------------------------------

#include "m+mMovementDbClient.hpp"
#include "m+mMovementDbRequests.hpp"

#include <m+m/m+mServiceResponse.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the client of the movement database service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::MovementDb;

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

MovementDbClient::MovementDbClient(void) :
    inherited("movementdb_")
{
    ODL_ENTER(); //####
    ODL_EXIT_P(this); //####
} // MovementDbClient::MovementDbClient

MovementDbClient::~MovementDbClient(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // MovementDbClient::~MovementDbClient

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool
MovementDbClient::addFileToDb(const YarpString & filePath)
{
    ODL_OBJENTER(); //####
    ODL_S1s("filePath = ", filePath); //####
    bool okSoFar = false;

    try
    {
        yarp::os::Bottle parameters;
        ServiceResponse  response;

        reconnectIfDisconnected();
        parameters.addString(filePath);
        if (send(MpM_ADDFILE_REQUEST_, parameters, response))
        {
            // Check that we got a successful add!
            if (MpM_EXPECTED_ADDFILE_RESPONSE_SIZE_ == response.count())
            {
                yarp::os::Value theValue = response.element(0);

                if (theValue.isString())
                {
                    okSoFar = (theValue.toString() == MpM_OK_RESPONSE_);
                }
                else
                {
                    ODL_LOG("! (theValue.isString())"); //####
                }
            }
            else
            {
                ODL_LOG("! (MpM_EXPECTED_ADDFILE_RESPONSE_SIZE_ == response.count())"); //####
                ODL_S1s("response = ", response.asString()); //####
            }
        }
        else
        {
            ODL_LOG("! (send(MpM_ADDFILE_REQUEST_, parameters, response))"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // MovementDbClient::addFileToDb

bool
MovementDbClient::setDataTrackForDb(const YarpString & dataTrack)
{
    ODL_OBJENTER(); //####
    ODL_S1s("dataTrack = ", dataTrack); //####
    bool okSoFar = false;

    try
    {
        yarp::os::Bottle parameters;
        ServiceResponse  response;

        reconnectIfDisconnected();
        parameters.addString(dataTrack);
        if (send(MpM_SETDATATRACK_REQUEST_, parameters, response))
        {
            // Check that we got a successful set!
            if (MpM_EXPECTED_SETDATATRACK_RESPONSE_SIZE_ == response.count())
            {
                yarp::os::Value theValue = response.element(0);

                if (theValue.isString())
                {
                    okSoFar = (theValue.toString() == MpM_OK_RESPONSE_);
                }
                else
                {
                    ODL_LOG("! (theValue.isString())"); //####
                }
            }
            else
            {
                ODL_LOG("! (MpM_EXPECTED_SETDATATRACK_RESPONSE_SIZE_ == response.count())"); //####
                ODL_S1s("response = ", response.asString()); //####
            }
        }
        else
        {
            ODL_LOG("! (send(MpM_SETDATATRACK_REQUEST_, parameters, response))"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // MovementDbClient::setDataTrackForDb

bool
MovementDbClient::setEmailAddressForDb(const YarpString & emailAddress)
{
    ODL_OBJENTER(); //####
    ODL_S1s("emailAddress = ", emailAddress); //####
    bool okSoFar = false;

    try
    {
        yarp::os::Bottle parameters;
        ServiceResponse  response;

        reconnectIfDisconnected();
        parameters.addString(emailAddress);
        if (send(MpM_SETEMAIL_REQUEST_, parameters, response))
        {
            // Check that we got a successful set!
            if (MpM_EXPECTED_SETEMAIL_RESPONSE_SIZE_ == response.count())
            {
                yarp::os::Value theValue = response.element(0);

                if (theValue.isString())
                {
                    okSoFar = (theValue.toString() == MpM_OK_RESPONSE_);
                }
                else
                {
                    ODL_LOG("! (theValue.isString())"); //####
                }
            }
            else
            {
                ODL_LOG("! (MpM_EXPECTED_SETEMAIL_RESPONSE_SIZE_ == response.count())"); //####
                ODL_S1s("response = ", response.asString()); //####
            }
        }
        else
        {
            ODL_LOG("! (send(MpM_SETEMAIL_REQUEST_, parameters, response))"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // MovementDbClient::setEmailAddressForDb

bool
MovementDbClient::stopDbConnection(void)
{
    ODL_OBJENTER(); //####
    bool okSoFar = false;

    try
    {
        yarp::os::Bottle parameters;
#if defined(MpM_DoExplicitCheckForOK)
        ServiceResponse  response;
#endif // defined(MpM_DoExplicitCheckForOK)

        reconnectIfDisconnected();
#if defined(MpM_DoExplicitCheckForOK)
        if (send(MpM_STOPDB_REQUEST_, parameters, response))
        {
            if (MpM_EXPECTED_STOPDB_RESPONSE_SIZE_ == response.count())
            {
                yarp::os::Value retrieved(response.element(0));

                if (retrieved.isString())
                {
                    okSoFar = (retrieved.toString() == MpM_OK_RESPONSE_);
                }
                else
                {
                    ODL_LOG("! (retrieved.isString())"); //####
                }
            }
            else
            {
                ODL_LOG("! (MpM_EXPECTED_STOPDB_RESPONSE_SIZE_ == response.count())"); //####
                ODL_S1s("response = ", response.asString()); //####
            }
        }
        else
        {
            ODL_LOG("! (send(MpM_STOPDB_REQUEST_, parameters, response))"); //####
        }
#else // ! defined(MpM_DoExplicitCheckForOK)
        if (send(MpM_STOPDB_REQUEST_, parameters))
        {
            okSoFar = true;
        }
        else
        {
            ODL_LOG("! (send(MpM_STOPDB_REQUEST_, parameters))"); //####
        }
#endif // ! defined(MpM_DoExplicitCheckForOK)
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(okSoFar);
    return okSoFar;
} // MovementDbClient::stopDbConnection


#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
