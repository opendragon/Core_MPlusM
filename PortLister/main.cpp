//--------------------------------------------------------------------------------------
//
//  File:       PortLister/main.cpp
//
//  Project:    MoAndMe
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

#include "MoMeBaseClient.h"
#include "MoMeRequests.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

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
#include <yarp/os/impl/BufferedConnectionWriter.h>
#include <yarp/os/impl/PortCommand.h>
#include <yarp/os/impl/Protocol.h>
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

/*! @brief The part name being used for probing connections. */
static const char * kMagicName = "<!!!>";

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Process the response from the name server.
 
 Note that each line of the response, except the last, is started with 'registration name'.
 @param received The response to be processed.
 @param ports The list of non-default ports found. */
static void processResponse(const yarp::os::ConstString &   received,
                            MoAndMe::Common::StringVector & ports)
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

/*! @brief Check if the Registry Service is active.
 @param ports The set of detected ports.
 @returns @c true if the Registry Service port is present and @c false otherwise. */
static bool checkForRegistryService(const MoAndMe::Common::StringVector & ports)
{
    OD_LOG_ENTER();//####
    bool result = false;
    
    for (MoAndMe::Common::StringVector::const_iterator it(ports.cbegin()); (! result) && (ports.cend() != it); ++it)
    {
        if (*it == MAM_SERVICE_REGISTRY_CHANNEL_NAME)
        {
            result = true;
        }
    }
    OD_LOG_EXIT_B(result);//####
    return result;
} // checkForRegistryService

/*! @brief Check if the response is for an input connection.
 @param response The response from the port that is being checked.
 @returns @c true if the response was for an input connection and @c false otherwise. */
static bool checkForInputConnection(const yarp::os::Bottle & response)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("response = ", response.toString().c_str());//####
    bool         sawConnection = false;
    const char * matchString[] = { "There", "is", "an", "input", "connection", "from", NULL, "to", NULL };
    int          respLen = response.size();
    int          matchLen = (sizeof(matchString) / sizeof(*matchString));
    
    if (respLen > matchLen)
    {
        bool matched = true;
        
        for (int ii = 0; matched && (ii < matchLen); ++ii)
        {
            yarp::os::ConstString element(response.get(ii).asString());
            
            if (matchString[ii])
            {
                if (element != matchString[ii])
                {
                    matched = false;
                }
            }
        }
        if (matched)
        {
            yarp::os::ConstString destination(response.get(matchLen - 1).asString());
            yarp::os::ConstString source(response.get(matchLen - 3).asString());
            
            if ((source != kMagicName) && (destination != kMagicName))
            {
                cout << "   Input from " << source.c_str() << endl;
                sawConnection = true;
            }
        }
    }
    OD_LOG_EXIT_B(sawConnection);//####
    return sawConnection;
} // checkForInputConnection

/*! @brief Check if the response is for an output connection.
 @param response The response from the port that is being checked.
 @returns @c true if the response was for an output connection and @c false otherwise. */
static bool checkForOutputConnection(const yarp::os::Bottle & response)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("response = ", response.toString().c_str());//####
    bool         sawConnection = false;
    const char * matchString[] = { "There", "is", "an", "output", "connection", "from", NULL, "to", NULL };
    int          respLen = response.size();
    int          matchLen = (sizeof(matchString) / sizeof(*matchString));

    if (respLen > matchLen)
    {
        bool matched = true;
        
        for (int ii = 0; matched && (ii < matchLen); ++ii)
        {
            yarp::os::ConstString element(response.get(ii).asString());
            
            if (matchString[ii])
            {
                if (element != matchString[ii])
                {
                    matched = false;
                }
            }
        }
        if (matched)
        {
            yarp::os::ConstString destination(response.get(matchLen - 1).asString());
            yarp::os::ConstString source(response.get(matchLen - 3).asString());
            
            if ((source != kMagicName) && (destination != kMagicName))
            {
                cout << "   Output to " << destination.c_str() << endl;
                sawConnection = true;
            }
        }
    }
    OD_LOG_EXIT_B(sawConnection);//####
    return sawConnection;
} // checkForOutputConnection

/*! @brief Report the connections for a given port.
 @param portName The port to be inspected. */
static void reportConnections(const std::string & portName)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("portName = ", portName.c_str());//####
    yarp::os::Contact address = yarp::os::Network::queryName(portName.c_str());

    if (address.isValid())
    {
        if (address.getCarrier() == "tcp")
        {
            yarp::os::impl::Address          fromAddress(yarp::os::impl::Address::fromContact(address));
            yarp::os::impl::OutputProtocol * out = yarp::os::impl::Carriers::connect(fromAddress);
            
            if (out)
            {
                yarp::os::impl::Route rr(kMagicName, portName.c_str(), "text_ack");
                
                if (out->open(rr))
                {
                    bool                                     sawInput = false;
                    bool                                     sawOutput = false;
                    yarp::os::Bottle                         resp;
                    yarp::os::impl::BufferedConnectionWriter bw(out->getConnection().isTextMode());
                    yarp::os::InputStream &                  is = out->getInputStream();
                    yarp::os::OutputStream &                 os = out->getOutputStream();
                    yarp::os::impl::PortCommand              pc(0, "*");
                    yarp::os::impl::StreamConnectionReader   reader;
                    
                    pc.write(bw);
                    bw.write(os);
                    reader.reset(is, NULL, rr, 0, true);
                    for (bool done = false; ! done; )
                    {
                        resp.read(reader);
                        yarp::os::ConstString checkString(resp.get(0).asString());
                        
                        if (checkString == "<ACK>")
                        {
                            done = true;
                        }
                        else if (checkString == "There")
                        {
                            if (checkForInputConnection(resp))
                            {
                                sawInput = true;
                            }
                            else if (checkForOutputConnection(resp))
                            {
                                sawOutput = true;
                            }
                        }
                    }
                    if ((! sawInput) && (! sawOutput))
                    {
                        cout << "   No active connections." << endl;
                    }
                }
                delete out;
            }
        }
    }
    OD_LOG_EXIT();//####
} // reportConnections

/*! @brief Print out connection information for a port.
 @param portName The name of the port of interest. */
static void reportPortStatus(const std::string & portName,
                             const bool          checkWithRegistry)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("portName = ", portName.c_str());//####
    OD_LOG_B1("checkWithRegistry = ", checkWithRegistry);//####
    if (checkWithRegistry)
    {
        std::string request(MAM_REQREP_DICT_CHANNELNAME_KEY ":");
        
        request += portName;
        MoAndMe::Common::Package matches(MoAndMe::Common::FindMatchingServices(request.c_str()));
        
        OD_LOG_S1("matches <- ", matches.toString().c_str());//####
        
        if (MAM_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())
        {
            yarp::os::ConstString matchesFirstString(matches.get(0).toString());
            
            if (! strcmp(MAM_OK_RESPONSE, matchesFirstString.c_str()))
            {
                cout << "   A service port." << endl;
            }
        }
    }
    reportConnections(portName);
    OD_LOG_EXIT();//####
} // reportPortStatus

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for creating an example client.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
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
            yarp::os::Network              yarp; // This is necessary to establish any connection to the YARP
                                                 // infrastructure
            MoAndMe::Common::Package       request;
            MoAndMe::Common::Package       response;
            yarp::os::ContactStyle         contactInfo;
            MoAndMe::Common::StringVector  ports;
            
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
                        bool serviceRegistryPresent = checkForRegistryService(ports);
                        
                        for (MoAndMe::Common::StringVector::const_iterator it(ports.cbegin()); ports.cend() != it; ++it)
                        {
                            if (! found)
                            {
                                cout << "Ports:" << endl << endl;
                                found = true;
                            }
                            cout << it->c_str() << endl;
                            reportPortStatus(*it, serviceRegistryPresent);
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
