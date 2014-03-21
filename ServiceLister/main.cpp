//--------------------------------------------------------------------------------------
//
//  File:       ServiceLister/main.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   A utility application to list the available services.
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
//  Created:    2014-03-12
//
//--------------------------------------------------------------------------------------

//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPBaseClient.h"
#include "YPPRequests.h"
#include "YPPServiceRequest.h"
#include <iostream>
#include <yarp/os/all.h>

using std::cout;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Retrieve the details for a service.
 @param aServicePort The port for the service.
 @param canonicalName The canonical name for the service.
 @param description The description of the service.
 @returns @c true if the service returned the desired information and @c false otherwise. */
static bool getNameAndDescriptionForService(const yarp::os::ConstString & aServicePort,
                                            yarp::os::ConstString &       canonicalName,
                                            yarp::os::ConstString &       description)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("aServicePort = ", aServicePort.c_str());//####
    bool                  result = false;
    yarp::os::ConstString aName(YarpPlusPlus::GetRandomPortName("/servicelister/port_"));
    yarp::os::Port *      newPort = new yarp::os::Port();
    
    if (newPort)
    {
        if (newPort->open(aName))
        {
            if (yarp::os::Network::connect(aName, aServicePort))
            {
                yarp::os::Bottle              parameters;
                YarpPlusPlus::ServiceRequest  request(YPP_NAME_REQUEST, parameters);
                YarpPlusPlus::ServiceResponse response;
                
                if (request.send(*newPort, &response))
                {
                    OD_SYSLOG_S1("response <- ", response.asString().c_str());//####
                    if (YPP_EXPECTED_NAME_RESPONSE_SIZE == response.count())
                    {
                        yarp::os::Value theCanonicalName(response.element(0));
                        yarp::os::Value theDescription(response.element(1));
                        
                        OD_SYSLOG_S2("theCanonicalName <- ", theCanonicalName.toString().c_str(),//####
                                     "theDescription <- ", theDescription.toString().c_str());//####
                        if (theCanonicalName.isString() && theDescription.isString())
                        {
                            canonicalName = theCanonicalName.toString();
                            description = theDescription.toString();
                            result = true;
                        }
                        else
                        {
                            OD_SYSLOG("! (theCanonicalName.isString() && theDescription.isString())");//####
                        }
                    }
                    else
                    {
                        OD_SYSLOG("! (YPP_EXPECTED_NAME_RESPONSE_SIZE == response.count())");//####
                    }
                }
                else
                {
                    OD_SYSLOG("! (request.send(*newPort, &response))");//####
                }
                if (! yarp::os::Network::disconnect(aName, aServicePort))
                {
                    OD_SYSLOG("(! yarp::os::Network::disconnect(aName, destinationName))");//####
                }
            }
            else
            {
                OD_SYSLOG("! (yarp::os::Network::connect(aName, destinationName))");//####
            }
            newPort->close();
        }
        else
        {
            OD_SYSLOG("! (newPort->open(portPath))");//####
        }
        delete newPort;
    }
    else
    {
        OD_SYSLOG("! (newPort)");//####
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // getNameAndDescriptionForService

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
#if defined(ENABLE_OD_SYSLOG)
# pragma unused(argc)
#else // ! defined(ENABLE_OD_SYSLOG)
# pragma unused(argc,argv)
#endif // ! defined(ENABLE_OD_SYSLOG)
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID |//####
                   kODSyslogOptionEnableThreadSupport | kODSyslogOptionWriteToStderr);//####
    OD_SYSLOG_ENTER();//####
    try
    {
        if (yarp::os::Network::checkNetwork())
        {
#if (defined(ENABLE_OD_SYSLOG) && defined(DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network::setVerbosity(1);
#else // ! (defined(ENABLE_OD_SYSLOG) && defined(DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network::setVerbosity(-1);
#endif // ! (defined(ENABLE_OD_SYSLOG) && defined(DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
            
            YarpPlusPlus::Initialize();
            yarp::os::Bottle matches(YarpPlusPlus::FindMatchingServices("request:*"));
            
            if (YPP_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())
            {
                // First, check if the search succeeded.
                yarp::os::ConstString matchesFirstString(matches.get(0).toString());
                
                if (strcmp(YPP_OK_RESPONSE, matchesFirstString.c_str()))
                {
                    OD_SYSLOG("(strcmp(YPP_OK_RESPONSE, matchesFirstString.c_str()))");//####
                    yarp::os::ConstString reason(matches.get(1).toString());
                    
                    cerr << "Failed: " << reason.c_str() << "." << endl;
                }
                else
                {
                    // Now, process the second element.
                    yarp::os::Bottle * matchesList = matches.get(1).asList();
                    
                    if (matchesList)
                    {
                        bool reported = false;
                        int  matchesCount = matchesList->size();
                        
                        if (matchesCount)
                        {
                            for (int ii = 0; ii < matchesCount; ++ii)
                            {
                                yarp::os::ConstString aMatch(matchesList->get(ii).toString());
                                yarp::os::ConstString canonicalName;
                                yarp::os::ConstString description;
                                
                                if (getNameAndDescriptionForService(aMatch, canonicalName, description))
                                {
                                    if (! reported)
                                    {
                                        cout << "Services: " << endl;
                                    }
                                    reported = true;
                                    cout << endl;
                                    cout << "Service port: " << aMatch.c_str() << endl;
                                    cout << "Service name: " << canonicalName.c_str() << endl;
                                    cout << "Description:  " << description.c_str() << endl;
                                }
                            }
                            cout << endl;
                        }
                        if (! reported)
                        {
                            cout << "No services found." << endl;
                        }
                    }
                    else
                    {
                        OD_SYSLOG("! (matchesList)");//####
                    }
                }
            }
            else
            {
                OD_SYSLOG("! (YPP_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())");//####
                cerr << "Problem getting information from the Service Registry." << endl;
            }
        }
        else
        {
            OD_SYSLOG("! (yarp::os::Network::checkNetwork())");//####
            cerr << "YARP network not running." << endl;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
    }
    yarp::os::Network::fini();
    OD_SYSLOG_EXIT_L(0);//####
    return 0;
} // main
