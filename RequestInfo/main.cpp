//--------------------------------------------------------------------------------------
//
//  File:       RequestInfo/main.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   A utility application to list the available requests.
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
//  Created:    2014-03-13
//
//--------------------------------------------------------------------------------------

//#define OD_ENABLE_LOGGING /* */
#include "ODLogging.h"
#include "YPPBaseClient.h"
#include "YPPRequests.h"
#include "YPPServiceRequest.h"
#include <iostream>
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#include <yarp/os/all.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using std::cout;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

static bool processResponse(const yarp::os::ConstString &         serviceName,
                            const YarpPlusPlus::ServiceResponse & response)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("response = ", &response);//####
    bool result = false;
    
    for (int ii = 0, howMany = response.count(); ii < howMany; ++ii)
    {
        yarp::os::Value element(response.element(ii));
        
        if (element.isDict())
        {
            yarp::os::Property * propList = element.asDict();
            
            if (propList)
            {
                if (propList->check(YPP_REQREP_DICT_REQUEST_KEY))
                {
                    yarp::os::ConstString theDetailsString;
                    yarp::os::ConstString theInputsString;
                    yarp::os::ConstString theOutputsString;
                    yarp::os::ConstString theVersionString;
                    yarp::os::ConstString theRequest(propList->find(YPP_REQREP_DICT_REQUEST_KEY).asString());
                    yarp::os::Bottle      keywordList;
                    
                    result = true;
                    if (propList->check(YPP_REQREP_DICT_DETAILS_KEY))
                    {
                        yarp::os::Value theDetails = propList->find(YPP_REQREP_DICT_DETAILS_KEY);
                        
                        if (theDetails.isString())
                        {
                            theDetailsString = theDetails.toString();
                        }
                    }
                    if (propList->check(YPP_REQREP_DICT_INPUT_KEY))
                    {
                        yarp::os::Value theInputs = propList->find(YPP_REQREP_DICT_INPUT_KEY);
                        
                        if (theInputs.isString())
                        {
                            theInputsString = theInputs.toString();
                        }
                    }
                    if (propList->check(YPP_REQREP_DICT_KEYWORDS_KEY))
                    {
                        yarp::os::Value theKeywords = propList->find(YPP_REQREP_DICT_KEYWORDS_KEY);
                        
                        if (theKeywords.isList())
                        {
                            keywordList = *theKeywords.asList();
                        }
                    }
                    if (propList->check(YPP_REQREP_DICT_OUTPUT_KEY))
                    {
                        yarp::os::Value theOutputs = propList->find(YPP_REQREP_DICT_OUTPUT_KEY);
                        
                        if (theOutputs.isString())
                        {
                            theOutputsString = theOutputs.toString();
                        }
                    }
                    if (propList->check(YPP_REQREP_DICT_VERSION_KEY))
                    {
                        yarp::os::Value theVersion = propList->find(YPP_REQREP_DICT_VERSION_KEY);
                        
                        if (theVersion.isString() || theVersion.isInt() || theVersion.isDouble())
                        {
                            theVersionString = theVersion.toString();
                        }
                    }
                    cout <<     "Service Port: " << serviceName.c_str() << endl;
                    cout <<     "Request:      " << theRequest.c_str() << endl;
                    if (0 < theVersionString.length())
                    {
                        cout << "Version:      " << theVersionString.c_str() << endl;
                    }
                    if (0 < theDetailsString.length())
                    {
                        cout << "Details:      " << theDetailsString.c_str() << endl;
                    }
                    if (0 < keywordList.size())
                    {
                        cout << "Keywords:     " << keywordList.toString().c_str() << endl;
                    }
                    if (0 < theInputsString.length())
                    {
                        cout << "Inputs:       " << theInputsString.c_str() << endl;
                    }
                    if (0 < theInputsString.length())
                    {
                        cout << "Outputs:      " << theOutputsString.c_str() << endl;
                    }
                    cout << endl;
                }
            }
        }
    }
    OD_LOG_EXIT_B(result);//####
    return result;
} // processResponse

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for creating an example client.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID |//####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr);//####
    OD_LOG_ENTER();//####
    try
    {
        if (yarp::os::Network::checkNetwork())
        {
#if (defined(OD_ENABLE_LOGGING) && defined(YPP_DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network::setVerbosity(1);
#else // ! (defined(OD_ENABLE_LOGGING) && defined(YPP_DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network::setVerbosity(-1);
#endif // ! (defined(OD_ENABLE_LOGGING) && defined(YPP_DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network     yarp; // This is necessary to establish any connection to the YARP infrastructure
            yarp::os::ConstString portNameRequest("portname:");
            const char *          requestName;
            
            YarpPlusPlus::Initialize();
            if (1 < argc)
            {
                portNameRequest += argv[1];
                if (2 < argc)
                {
                    if (strcmp(argv[2], "*"))
                    {
                        requestName = argv[2];
                    }
                    else
                    {
                        requestName = NULL;
                    }
                }
                else
                {
                    requestName = NULL;
                }
            }
            else
            {
                portNameRequest += "*";
                requestName = NULL;
            }
            yarp::os::Bottle matches(YarpPlusPlus::FindMatchingServices(portNameRequest));
            
            if (YPP_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())
            {
                // First, check if the search succeeded.
                yarp::os::ConstString matchesFirstString(matches.get(0).toString());
                
                if (strcmp(YPP_OK_RESPONSE, matchesFirstString.c_str()))
                {
                    OD_LOG("(strcmp(YPP_OK_RESPONSE, matchesFirstString.c_str()))");//####
                    yarp::os::ConstString reason(matches.get(1).toString());
                    
                    cerr << "Failed: " << reason.c_str() << "." << endl;
                }
                else
                {
                    // Now, process the second element.
                    yarp::os::Bottle * matchesList = matches.get(1).asList();
                    
                    if (matchesList)
                    {
                        int matchesCount = matchesList->size();
                        
                        if (matchesCount)
                        {
                            yarp::os::ConstString aName(YarpPlusPlus::GetRandomPortName("/requestinfo/port_"));
                            yarp::os::Port *      newPort = new yarp::os::Port;
                            
                            if (newPort)
                            {
                                if (newPort->open(aName))
                                {
                                    bool             sawRequestResponse = false;
                                    yarp::os::Bottle parameters;
                                    
                                    if (requestName)
                                    {
                                        parameters = requestName;
                                    }
                                    for (int ii = 0; ii < matchesCount; ++ii)
                                    {
                                        yarp::os::ConstString aMatch(matchesList->get(ii).toString());
                                        
                                        if (yarp::os::Network::connect(aName, aMatch))
                                        {
                                            YarpPlusPlus::ServiceResponse response;
                                            
                                            // If no request was identified, or a wildcard was specified, we use the
                                            // 'list' request; otherwise, do an 'info' request.
                                            if (requestName)
                                            {
                                                YarpPlusPlus::ServiceRequest request(YPP_INFO_REQUEST, parameters);
                                                
                                                if (request.send(*newPort, &response))
                                                {
                                                    if (0 < response.count())
                                                    {
                                                        if (processResponse(aMatch, response))
                                                        {
                                                            sawRequestResponse = true;
                                                        }
                                                    }
                                                }
                                                else
                                                {
                                                    OD_LOG("! (request.send(*newPort, &response))");//####
                                                    cerr << "Problem communicating with " << aMatch.c_str() << "." <<
                                                            endl;
                                                }
                                            }
                                            else
                                            {
                                                YarpPlusPlus::ServiceRequest request(YPP_LIST_REQUEST, parameters);
                                                
                                                if (request.send(*newPort, &response))
                                                {
                                                    if (0 < response.count())
                                                    {
                                                        if (processResponse(aMatch, response))
                                                        {
                                                            sawRequestResponse = true;
                                                        }
                                                    }
                                                }
                                                else
                                                {
                                                    OD_LOG("! (request.send(*newPort, &response))");//####
                                                    cerr << "Problem communicating with " << aMatch.c_str() << "." <<
                                                            endl;
                                                }
                                            }
                                            if (! yarp::os::Network::disconnect(aName, aMatch))
                                            {
                                                OD_LOG("(! yarp::os::Network::disconnect(aName, aMatch))");//####
                                            }
                                        }
                                        else
                                        {
                                            OD_LOG("! (yarp::os::Network::connect(aName, aMatch))");//####
                                        }
                                    }
                                    if (! sawRequestResponse)
                                    {
                                        cout << "No matching request found." << endl;
                                    }
                                    newPort->close();
                                }
                                else
                                {
                                    OD_LOG("! (newPort->open(portPath))");//####
                                }
                                delete newPort;
                            }
                            else
                            {
                                OD_LOG("! (newPort)");//####
                            }
                        }
                        else
                        {
                            cout << "No services found." << endl;
                        }
                    }
                    else
                    {
                        OD_LOG("! (matchesList)");//####
                    }
                }
            }
            else
            {
                OD_LOG("! (YPP_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())");//####
                cerr << "Problem getting information from the Service Registry." << endl;
            }
        }
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork())");//####
            cerr << "YARP network not running." << endl;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0);//####
    return 0;
} // main
