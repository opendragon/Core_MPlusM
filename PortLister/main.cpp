//--------------------------------------------------------------------------------------
//
//  File:       PortLister/main.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   A utility application to list the available ports.
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
//  Created:    2014-03-28
//
//--------------------------------------------------------------------------------------

//#include "ODEnableLogging.h"
#include "ODLogging.h"
#include "YPPCommon.h"
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

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief A utility application to list the available ports. */

/*! @dir PortLister
 @brief The PortLister application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using std::cout;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The indicator string for the beginning of new information received. */
static const char * kLineMarker = "registration name ";

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Process the response from the name server.
 
 Note that each line of the response, except the last, is started with 'registration name'.
 @param received The response to be processed.
 @param ports The list of non-default ports found. */
static void processResponse(const yarp::os::ConstString & received,
                            YarpPlusPlus::StringVector &  ports)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("received = ", received.c_str());//####
    int                   lineMakerLength = static_cast<int>(strlen(kLineMarker));
    yarp::os::ConstString nameServerName(yarp::os::Network::getNameServerName());
    yarp::os::ConstString workingCopy(received);

    OD_LOG_S1("nameServerName = ", nameServerName.c_str());//####
    for (int nextPos = 0; yarp::os::ConstString::npos != nextPos; )
    {
        nextPos = workingCopy.find(kLineMarker);
        if (yarp::os::ConstString::npos != nextPos)
        {
            workingCopy = workingCopy.substr(nextPos + lineMakerLength);
            int endPos = workingCopy.find(" ");
            
            if (yarp::os::ConstString::npos == endPos)
            {
                nextPos = yarp::os::ConstString::npos;
            }
            else
            {
                yarp::os::ConstString channelName(workingCopy.substr(0, endPos));
                
                if ('/' == channelName[0])
                {
                    if (channelName != nameServerName)
                    {
                        ports.push_back(channelName.c_str());
                    }
                }
            }
        }
    }
    OD_LOG_EXIT();//####
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
#if defined(OD_ENABLE_LOGGING)
# pragma unused(argc)
#else // ! defined(OD_ENABLE_LOGGING)
# pragma unused(argc,argv)
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID |//####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr);//####
    OD_LOG_ENTER();//####
    try
    {
        if (yarp::os::Network::checkNetwork())
        {
            yarp::os::Network          yarp; // This is necessary to establish any connection to the YARP infrastructure
            YarpPlusPlus::Package      request;
            YarpPlusPlus::Package      response;
            yarp::os::ContactStyle     contactInfo;
            YarpPlusPlus::StringVector ports;
            
            request.addString("list");
            contactInfo.timeout = 5.0;
            if (yarp::os::Network::writeToNameServer(request, response, contactInfo))
            {
                if (1 == response.size())
                {
                    yarp::os::Value responseValue(response.get(0));
                    
                    if (responseValue.isString())
                    {
                        bool found = false;
                        
                        processResponse(responseValue.asString(), ports);
                        for (YarpPlusPlus::StringVector::const_iterator it(ports.cbegin()); ports.cend() != it;
                             ++it)
                        {
                            if (! found)
                            {
                                cout << "Ports:" << endl << endl;
                                found = true;
                            }
                            cout << it->c_str() << endl;
                        }
                        if (! found)
                        {
                            cout << "No ports found." << endl;
                        }
                    }
                    else
                    {
                        OD_LOG("! (responseValue.isString())");//####
                    }
                }
                else
                {
                    OD_LOG("! (1 == response.size())");//####
                    OD_LOG_S1("response = ", response.toString().c_str());//####
                }
            }
            else
            {
                OD_LOG("! (yarp::os::Network::writeToNameServer(request, response))");//####
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
