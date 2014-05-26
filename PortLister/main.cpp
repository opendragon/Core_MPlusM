//--------------------------------------------------------------------------------------
//
//  File:       PortLister/main.cpp
//
//  Project:    M+M
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

#include "M+MBaseClient.h"
#include "M+MRequests.h"
#include "M+MUtilities.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

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

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Report the connections for a given port.
 @param portName The port to be inspected.
 @param quiet @c true if status output is to be suppressed and @c false otherwise. */
static void reportConnections(const yarp::os::ConstString & portName,
                              const bool                    quiet = false)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("portName = ", portName.c_str());//####
    OD_LOG_B1("quiet = ", quiet);//####
    MplusM::Common::StringVector inputs;
    MplusM::Common::StringVector outputs;

    MplusM::Utilities::GatherPortConnections(portName, inputs, outputs, MplusM::Utilities::kInputAndOutputBoth, quiet);
    if ((0 < inputs.size()) || (0 < outputs.size()))
    {
        for (int ii = 0, mm = inputs.size(); mm > ii; ++ii)
        {
            yarp::os::ConstString aConnection = inputs[ii];
            
            cout << "   Input from " << aConnection.c_str() << "." << endl;
        }
        for (int ii = 0, mm = outputs.size(); mm > ii; ++ii)
        {
            yarp::os::ConstString aConnection = outputs[ii];
            
            cout << "   Output to " << aConnection.c_str() << "." << endl;
        }
    }
    else
    {
        cout << "   No active connections." << endl;
    }
    OD_LOG_EXIT();//####
} // reportConnections

/*! @brief Print out connection information for a port.
 @param aDescriptor The attributes of the port of interest.
 @param checkWithRegistry @c true if the Service Registry is available for requests and @c false otherwise. */
static void reportPortStatus(const MplusM::Utilities::PortDescriptor & aDescriptor,
                             const bool                                checkWithRegistry)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("aDescriptor = ", &aDescriptor);//####
    OD_LOG_B1("checkWithRegistry = ", checkWithRegistry);//####
    const size_t kAdapterPortNameBaseLen = sizeof(ADAPTER_PORT_NAME_BASE) - 1;
    const size_t kClientPortNameBaseLen = sizeof(CLIENT_PORT_NAME_BASE) - 1;
    const size_t kDefaultServiceNameBaseLen = sizeof(DEFAULT_SERVICE_NAME_BASE) - 1;
    const char * portNameChars = aDescriptor._portName.c_str();

    cout << portNameChars << ": ";
    if (checkWithRegistry)
    {
        yarp::os::ConstString request(MpM_REQREP_DICT_CHANNELNAME_KEY ":");
        
        request += portNameChars;
        MplusM::Common::Package matches(MplusM::Common::FindMatchingServices(request.c_str(), true));
        
        OD_LOG_S1("matches <- ", matches.toString().c_str());//####
        if (MpM_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())
        {
            yarp::os::ConstString matchesFirstString(matches.get(0).toString());
            
            if (strcmp(MpM_OK_RESPONSE, matchesFirstString.c_str()))
            {
                // Didn't match - use a simpler check, in case it's unregistered or is an adapter or client.
                if (! strncmp(DEFAULT_SERVICE_NAME_BASE, portNameChars, kDefaultServiceNameBaseLen))
                {
                    cout << "Unregistered service port.";
                }
                else if (! strncmp(ADAPTER_PORT_NAME_BASE, portNameChars, kAdapterPortNameBaseLen))
                {
                    cout << "Adapter port.";
                }
                else if (! strncmp(CLIENT_PORT_NAME_BASE, portNameChars, kClientPortNameBaseLen))
                {
                    cout << "Client port.";
                }
                else
                {
                    // A plain port.
                    cout << "Standard port at " << aDescriptor._portIpAddress.c_str() << ":" <<
                            aDescriptor._portPortNumber.c_str();
                }
            }
            else
            {
                yarp::os::Value secondValue(matches.get(1));
                
                if (secondValue.isList())
                {
                    MplusM::Common::Package * secondList = secondValue.asList();
                    
                    if (secondList && secondList->size())
                    {
                        yarp::os::ConstString serviceName(matches.get(1).toString());
                        
                        if (aDescriptor._portName == MpM_REGISTRY_CHANNEL_NAME)
                        {
                            cout << "Service registry port for '" << serviceName.c_str() << "'.";
                        }
                        else
                        {
                            cout << "Service port for '" << serviceName.c_str() << "'.";
                        }
                    }
                    else
                    {
                        // The response was an empty list - use a simpler check, in case it's unregistered or is an
                        // adapter or client.
                        if (! strncmp(DEFAULT_SERVICE_NAME_BASE, portNameChars, kDefaultServiceNameBaseLen))
                        {
                            cout << "Unregistered service port.";
                        }
                        else if (! strncmp(ADAPTER_PORT_NAME_BASE, portNameChars, kAdapterPortNameBaseLen))
                        {
                            cout << "Adapter port.";
                        }
                        else if (! strncmp(CLIENT_PORT_NAME_BASE, portNameChars, kClientPortNameBaseLen))
                        {
                            cout << "Client port.";
                        }
                        else
                        {
                            // A plain port.
                            cout << "Standard port at " << aDescriptor._portIpAddress.c_str() << ":" <<
                                    aDescriptor._portPortNumber.c_str();
                        }
                    }
                }
            }
        }
        MplusM::Common::StringVector inputs;
        MplusM::Common::StringVector outputs;
        bool                         isPrimary;
        
        if (MplusM::Utilities::GetAssociatedPorts(aDescriptor._portName, inputs, outputs, isPrimary, true))
        {
            if (isPrimary)
            {
                cout << " Primary port with inputs (";
                for (int ii = 0, mm = inputs.size(); mm > ii; ++ii)
                {
                    if (ii)
                    {
                        cout << ", ";
                    }
                    cout << inputs[ii].c_str();
                }
                cout << ") and outputs (";
                for (int ii = 0, mm = outputs.size(); mm > ii; ++ii)
                {
                    if (ii)
                    {
                        cout << ", ";
                    }
                    cout << outputs[ii].c_str();
                }
                cout << ").";
            }
            else
            {
                cout << " Port associated with " << inputs[0].c_str() << ".";
            }
        }
    }
    else
    {
        // We can't interrogate the service registry, so use a simple heuristic to identify clients, services and
        // adapters.
        if (! strncmp(DEFAULT_SERVICE_NAME_BASE, portNameChars, kDefaultServiceNameBaseLen))
        {
            cout << "Unregistered service port.";
        }
        else if (! strncmp(ADAPTER_PORT_NAME_BASE, portNameChars, kAdapterPortNameBaseLen))
        {
            cout << "Adapter port.";
        }
        else if (! strncmp(CLIENT_PORT_NAME_BASE, portNameChars, kClientPortNameBaseLen))
        {
            cout << "Client port.";
        }
        else
        {
            // A plain port.
            cout << "Standard port at " << aDescriptor._portIpAddress.c_str() << ":" <<
                    aDescriptor._portPortNumber.c_str();
        }
    }
    cout << endl;
    reportConnections(aDescriptor._portName);
    OD_LOG_EXIT();//####
} // reportPortStatus

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for listing the connection status of all visible YARP ports.

 There is no input and the output consists of a list of ports and what, if anything, is connected to them.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
#if MAC_OR_LINUX_
# pragma unused(argc)
#endif // MAC_OR_LINUX_
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID |//####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr);//####
    OD_LOG_ENTER();//####
    try
    {
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork())
#endif // CheckNetworkWorks_
        {
            yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure

            MplusM::Common::Initialize(*argv);
            MplusM::Utilities::PortVector ports;
            bool                          found = false;
            
            MplusM::Utilities::GetDetectedPortList(ports);
            if (ports.size())
            {
                bool serviceRegistryPresent = MplusM::Utilities::CheckForRegistryService(ports);
                
                for (size_t ii = 0, mm = ports.size(); mm > ii; ++ii)
                {
                    MplusM::Utilities::PortDescriptor & aDescriptor = ports[ii];
                    
                    if (! found)
                    {
                        cout << "Ports:" << endl << endl;
                        found = true;
                    }
                    reportPortStatus(aDescriptor, serviceRegistryPresent);
                }
            }
            if (! found)
            {
                cout << "No ports found." << endl;
            }
        }
#if CheckNetworkWorks_
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork())");//####
            cerr << "YARP network not running." << endl;
        }
#endif // CheckNetworkWorks_
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0);//####
    return 0;
} // main
