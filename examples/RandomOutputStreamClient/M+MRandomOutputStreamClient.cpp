//--------------------------------------------------------------------------------------
//
//  File:       M+MRandomOutputStreamClient.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the client of a simple M+M service.
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
//  Created:    2014-06-24
//
//--------------------------------------------------------------------------------------

#include "M+MRandomOutputStreamClient.h"
#include "M+MRandomOutputStreamRequests.h"
#include "M+MServiceResponse.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the client of a simple M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;

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

RandomOutputStreamClient::RandomOutputStreamClient(void) :
        inherited("example/randomoutputstream_")
{
    OD_LOG_ENTER();//####
    OD_LOG_EXIT_P(this);//####
} // RandomOutputStreamClient::RandomOutputStreamClient

RandomOutputStreamClient::~RandomOutputStreamClient(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // RandomOutputStreamClient::~RandomOutputStreamClient

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

#if 0
bool RandomOutputStreamClient::getOneRandomNumber(double & result)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("result = ", &result);//####
    bool okSoFar = false;

    try
    {
        Common::Package         parameters;
        Common::ServiceResponse response;
        
        reconnectIfDisconnected();
        if (send(MpM_RANDOM_REQUEST, parameters, &response))
        {
            if (1 == response.count())
            {
                yarp::os::Value retrieved(response.element(0));
                
                if (retrieved.isDouble())
                {
                    result = retrieved.asDouble();
                    okSoFar = true;
                }
                else
                {
                    OD_LOG("! (retrieved.isDouble())");//####
                }
            }
            else
            {
                OD_LOG("! (1 == response.count())");//####
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
} // RandomOutputStreamClient::getOneRandomNumber
#endif//0

#if 0
bool RandomOutputStreamClient::getRandomNumbers(const int              howMany,
                                                Common::DoubleVector & result)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_LL1("howMany = ", howMany);//####
    OD_LOG_P1("result = ", &result);//####
    bool okSoFar = false;
    
    try
    {
        if (0 < howMany)
        {
            Common::Package         parameters;
            Common::ServiceResponse response;
            
            parameters.addInt(howMany);
            reconnectIfDisconnected();
            if (send(MpM_RANDOM_REQUEST, parameters, &response))
            {
                if (howMany == response.count())
                {
                    result.clear();
                    okSoFar = true;
                    for (int ii = 0; ii < howMany; ++ii)
                    {
                        yarp::os::Value retrieved(response.element(ii));
                        
                        if (retrieved.isDouble())
                        {
                            result.push_back(retrieved.asDouble());
                        }
                        else
                        {
                            OD_LOG("! (retrieved.isDouble())");//####
                            okSoFar = false;
                            break;
                        }
                        
                    }
                }
                else
                {
                    OD_LOG("! (howMany == response.count())");//####
                    OD_LOG_S1("response = ", response.asString().c_str());//####
                }
            }
            else
            {
                OD_LOG("! (send(MpM_RANDOM_REQUEST, parameters, &response))");//####
            }
        }
        else
        {
            OD_LOG("! (0 < howMany)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // RandomOutputStreamClient::getRandomNumbers
#endif//0

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
