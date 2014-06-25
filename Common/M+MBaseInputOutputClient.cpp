//--------------------------------------------------------------------------------------
//
//  File:       M+MBaseInputOutputClient.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the minimal functionality required for an M+M
//              input/output client.
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
//  Created:    2014-06-25
//
//--------------------------------------------------------------------------------------

#include "M+MBaseInputOutputClient.h"
#include "M+MRequests.h"
#include "M+MServiceResponse.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the minimal functionality required for an M+M
 input/output client. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

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

BaseInputOutputClient::BaseInputOutputClient(const char * baseChannelName) :
        inherited(baseChannelName)
{
    OD_LOG_ENTER();//####
    OD_LOG_EXIT_P(this);//####
} // BaseInputOutputClient::BaseInputOutputClient

BaseInputOutputClient::~BaseInputOutputClient(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // BaseInputOutputClient::~BaseInputOutputClient

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool BaseInputOutputClient::configure(const Package & details)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("result = ", &result);//####
    bool okSoFar = false;
    
    try
    {
        ServiceResponse response;
        
        reconnectIfDisconnected();
        if (send(MpM_CONFIGURE_REQUEST, details, &response))
        {
            if (MpM_EXPECTED_CONFIGURE_RESPONSE_SIZE == response.count())
            {
                // The first element of the response should be 'OK' or 'FAILED'.
                yarp::os::Value responseFirst(response.element(0));
                
                if (responseFirst.isString())
                {
                    yarp::os::ConstString responseFirstAsString(responseFirst.toString());
                    
                    if (! strcmp(MpM_OK_RESPONSE, responseFirstAsString.c_str()))
                    {
                        okSoFar = true;
                    }
                    else if (strcmp(MpM_FAILED_RESPONSE, responseFirstAsString.c_str()))
                    {
                        OD_LOG("strcmp(MpM_FAILED_RESPONSE, responseFirstAsString.c_str())");//####
                    }
                }
                else
                {
                    OD_LOG("! (responseFirst.isString())");//####
                }
            }
            else
            {
                OD_LOG("! (MpM_EXPECTED_CONFIGURE_RESPONSE_SIZE == response.count())");//####
                OD_LOG_S1("response = ", response.asString().c_str());//####
            }
        }
        else
        {
            OD_LOG("! (send(MpM_RANDOM_REQUEST, parameters, &response))");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // BaseInputOutputClient::configure

bool BaseInputOutputClient::restartStreams(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("result = ", &result);//####
    bool okSoFar = false;
    
    try
    {
        Package         parameters;
        ServiceResponse response;
        
        reconnectIfDisconnected();
        if (send(MpM_RESTARTSTREAMS_REQUEST, parameters, &response))
        {
            if (MpM_EXPECTED_RESTARTSTREAMS_RESPONSE_SIZE == response.count())
            {
                // The first element of the response should be 'OK' or 'FAILED'.
                yarp::os::Value responseFirst(response.element(0));
                
                if (responseFirst.isString())
                {
                    yarp::os::ConstString responseFirstAsString(responseFirst.toString());
                    
                    if (! strcmp(MpM_OK_RESPONSE, responseFirstAsString.c_str()))
                    {
                        okSoFar = true;
                    }
                    else if (strcmp(MpM_FAILED_RESPONSE, responseFirstAsString.c_str()))
                    {
                        OD_LOG("strcmp(MpM_FAILED_RESPONSE, responseFirstAsString.c_str())");//####
                    }
                }
                else
                {
                    OD_LOG("! (responseFirst.isString())");//####
                }
            }
            else
            {
                OD_LOG("! (MpM_EXPECTED_RESTARTSTREAMS_RESPONSE_SIZE == response.count())");//####
                OD_LOG_S1("response = ", response.asString().c_str());//####
            }
        }
        else
        {
            OD_LOG("! (send(MpM_RANDOM_REQUEST, parameters, &response))");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // BaseInputOutputClient::restartStreams

bool BaseInputOutputClient::startStreams(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("result = ", &result);//####
    bool okSoFar = false;
    
    try
    {
        Package         parameters;
        ServiceResponse response;
        
        reconnectIfDisconnected();
        if (send(MpM_STARTSTREAMS_REQUEST, parameters, &response))
        {
            if (MpM_EXPECTED_STARTSTREAMS_RESPONSE_SIZE == response.count())
            {
                // The first element of the response should be 'OK' or 'FAILED'.
                yarp::os::Value responseFirst(response.element(0));
                
                if (responseFirst.isString())
                {
                    yarp::os::ConstString responseFirstAsString(responseFirst.toString());
                    
                    if (! strcmp(MpM_OK_RESPONSE, responseFirstAsString.c_str()))
                    {
                        okSoFar = true;
                    }
                    else if (strcmp(MpM_FAILED_RESPONSE, responseFirstAsString.c_str()))
                    {
                        OD_LOG("strcmp(MpM_FAILED_RESPONSE, responseFirstAsString.c_str())");//####
                    }
                }
                else
                {
                    OD_LOG("! (responseFirst.isString())");//####
                }
            }
            else
            {
                OD_LOG("! (MpM_EXPECTED_STARTSTREAMS_RESPONSE_SIZE == response.count())");//####
                OD_LOG_S1("response = ", response.asString().c_str());//####
            }
        }
        else
        {
            OD_LOG("! (send(MpM_RANDOM_REQUEST, parameters, &response))");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // BaseInputOutputClient::startStreams

bool BaseInputOutputClient::stopStreams(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("result = ", &result);//####
    bool okSoFar = false;
    
    try
    {
        Package         parameters;
        ServiceResponse response;
        
        reconnectIfDisconnected();
        if (send(MpM_STOPSTREAMS_REQUEST, parameters, &response))
        {
            if (MpM_EXPECTED_STOPSTREAMS_RESPONSE_SIZE == response.count())
            {
                // The first element of the response should be 'OK' or 'FAILED'.
                yarp::os::Value responseFirst(response.element(0));
                
                if (responseFirst.isString())
                {
                    yarp::os::ConstString responseFirstAsString(responseFirst.toString());
                    
                    if (! strcmp(MpM_OK_RESPONSE, responseFirstAsString.c_str()))
                    {
                        okSoFar = true;
                    }
                    else if (strcmp(MpM_FAILED_RESPONSE, responseFirstAsString.c_str()))
                    {
                        OD_LOG("strcmp(MpM_FAILED_RESPONSE, responseFirstAsString.c_str())");//####
                    }
                }
                else
                {
                    OD_LOG("! (responseFirst.isString())");//####
                }
            }
            else
            {
                OD_LOG("! (MpM_EXPECTED_STOPSTREAMS_RESPONSE_SIZE == response.count())");//####
                OD_LOG_S1("response = ", response.asString().c_str());//####
            }
        }
        else
        {
            OD_LOG("! (send(MpM_RANDOM_REQUEST, parameters, &response))");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // BaseInputOutputClient::stopStreams

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
