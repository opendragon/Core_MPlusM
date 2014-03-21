//--------------------------------------------------------------------------------------
//
//  File:       YPPExampleRandomNumberClient.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for the client of a simple Yarp++ service.
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
//  Created:    2014-03-06
//
//--------------------------------------------------------------------------------------

#include "YPPExampleRandomNumberClient.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPExampleRandomNumberRequests.h"
#include "YPPServiceResponse.h"

using namespace YarpPlusPlusExample;

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

ExampleRandomNumberClient::ExampleRandomNumberClient(void) :
        inherited("example/randomnumber_")
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // ExampleRandomNumberClient::ExampleRandomNumberClient

ExampleRandomNumberClient::~ExampleRandomNumberClient(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    OD_SYSLOG_EXIT();//####
} // ExampleRandomNumberClient::~ExampleRandomNumberClient

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool ExampleRandomNumberClient::getOneRandomNumber(double & result)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    OD_SYSLOG_P1("result = ", &result);//####
    bool okSoFar = false;

    try
    {
        yarp::os::Bottle              parameters;
        YarpPlusPlus::ServiceResponse response;
        
        if (send(YPP_RANDOM_REQUEST, parameters, &response))
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
                    OD_SYSLOG("! (retrieved.isDouble())");//####
                }
            }
            else
            {
                OD_SYSLOG("! (1 == response.count())");//####
            }
        }
        else
        {
            OD_SYSLOG("! (send(YPP_RANDOM_REQUEST, parameters, &response))");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(okSoFar);//####
    return okSoFar;
} // ExampleRandomNumberClient::getOneRandomNumber

bool ExampleRandomNumberClient::getRandomNumbers(const int      howMany,
                                                 RandomVector & result)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    OD_SYSLOG_LL1("howMany = ", howMany);//####
    OD_SYSLOG_P1("result = ", &result);//####
    bool okSoFar = false;
    
    try
    {
        if (0 < howMany)
        {
            yarp::os::Bottle              parameters;
            YarpPlusPlus::ServiceResponse response;
            
            parameters.addInt(howMany);
            if (send(YPP_RANDOM_REQUEST, parameters, &response))
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
                            OD_SYSLOG("! (retrieved.isDouble())");//####
                            okSoFar = false;
                            break;
                        }
                        
                    }
                }
                else
                {
                    OD_SYSLOG("! (howMany == response.count())");//####
                }
            }
            else
            {
                OD_SYSLOG("! (send(YPP_RANDOM_REQUEST, parameters, &response))");//####
            }
        }
        else
        {
            OD_SYSLOG("! (0 < howMany)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(okSoFar);//####
    return okSoFar;
} // ExampleRandomNumberClient::getRandomNumbers

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
