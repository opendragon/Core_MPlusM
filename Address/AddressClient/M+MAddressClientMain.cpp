//--------------------------------------------------------------------------------------------------
//
//  File:       M+MAddressClientMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the client of the Address service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2015-02-11
//
//--------------------------------------------------------------------------------------------------

#include "M+MAddressClient.h"
#include "M+MAddressRequests.h"

#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file 
 @brief The main application for the client of the Address service. */

/*! @dir AddressClient
 @brief The set of files that implement the Address client. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Address;
using namespace MplusM::Common;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Process the argument list for the application.
 @param arguments The arguments to analyze.
 @param namePattern The generated search value.
 @param needsAddress @c true if the IP address is requested and @c false otherwise.
 @param needsPort @c true if the port is requested and @c false otherwise. */
static void processArguments(const YarpStringVector & arguments,
                             YarpString &             namePattern,
                             bool &                   needsAddress,
                             bool &                   needsPort)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P4("arguments = ", &arguments, "namePattern = ", &namePattern, "needsAddress = ", //####
              &needsAddress, "needsPort = ", &needsPort); //####
    YarpString tag;
    
    needsAddress = needsPort = true;
    for (int ii = 0, argc = arguments.size(); argc > ii; ++ii)
    {
        YarpString anArg = arguments[ii];
        
        if (anArg == "address")
        {
            needsAddress = true;
            needsPort = false;
        }
        else if (anArg == "both")
        {
            needsAddress = needsPort = true;
        }
        else if (anArg == "port")
        {
            needsAddress = false;
            needsPort = true;
        }
        else
        {
            tag = anArg;
        }
    }
    if (0 < tag.length())
    {
        YarpString singleQuote("'");

        namePattern = singleQuote + namePattern + " " + tag + singleQuote;
    }
    OD_LOG_EXIT(); //####
} // processArguments

/*! @brief Set up the environment and perform the operation.
 @param arguments The arguments to analyze.
 @param flavour The format for the output. */
#if defined(MpM_ReportOnConnections)
static void setUpAndGo(ChannelStatusReporter *  reporter,
                       const YarpStringVector & arguments,
                       const OutputFlavour      flavour)
#else // ! defined(MpM_ReportOnConnections)
static void setUpAndGo(const YarpStringVector & arguments,
                       const OutputFlavour      flavour)
#endif // ! defined(MpM_ReportOnConnections)
{
    OD_LOG_ENTER(); //####
#if defined(MpM_ReportOnConnections)
    OD_LOG_P2("reporter = ", reporter, "arguments = ", &arguments); //####
#else // ! defined(MpM_ReportOnConnections)
    OD_LOG_P1("arguments = ", &arguments); //####
#endif // ! defined(MpM_ReportOnConnections)
    YarpString      channelNameRequest(MpM_REQREP_DICT_NAME_KEY ":");
    YarpString      namePattern(MpM_ADDRESS_CANONICAL_NAME);
    AddressClient * stuff = new AddressClient;
    
    if (stuff)
    {
        bool needsAddress;
        bool needsPort;
        
#if defined(MpM_ReportOnConnections)
        stuff->setReporter(*reporter, true);
#endif // defined(MpM_ReportOnConnections)
        processArguments(arguments, namePattern, needsAddress, needsPort);
        channelNameRequest += namePattern;
        if (stuff->findService(channelNameRequest.c_str()))
        {
            if (stuff->connectToService())
            {
                YarpString address;
                int        port;
                
                if (stuff->getAddress(address, port))
                {
                    switch (flavour)
                    {
                        case kOutputFlavourJSON :
                            cout << "{ ";
                            if (needsAddress)
                            {
                                cout << T_(CHAR_DOUBLEQUOTE "Address" CHAR_DOUBLEQUOTE ": "
                                           CHAR_DOUBLEQUOTE);
                                cout << SanitizeString(address).c_str() << T_(CHAR_DOUBLEQUOTE);
                            }
                            if (needsPort)
                            {
                                if (needsAddress)
                                {
                                    cout << ", ";
                                }
                                cout << T_(CHAR_DOUBLEQUOTE "Address" CHAR_DOUBLEQUOTE ": "
                                           CHAR_DOUBLEQUOTE);
                                cout << port << T_(CHAR_DOUBLEQUOTE);
                            }
                            cout << " }" << endl;
                            break;
                            
                        case kOutputFlavourTabs :
                            if (needsAddress)
                            {
                                cout << address.c_str();
                            }
                            if (needsPort)
                            {
                                if (needsAddress)
                                {
                                    cout << "\t";
                                }
                                cout << port;
                            }
                            cout << endl;
                            break;
                            
                        case kOutputFlavourNormal :
                            if (needsAddress)
                            {
                                cout << "Address: " << address.c_str();
                            }
                            if (needsPort)
                            {
                                if (needsAddress)
                                {
                                    cout << ", ";
                                }
                                cout << "Port: " << port;
                            }
                            cout << endl;
                            break;
                            
                        default :
                            break;
                            
                    }
                }
                else
                {
                    OD_LOG("! (stuff->getAddress(address, port))"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("Problem fetching the address "
                                     "information.");
#endif // MAC_OR_LINUX_
                }
            }
            else
            {
                OD_LOG("! (stuff->connectToService())"); //####
#if MAC_OR_LINUX_
                GetLogger().fail("Could not connect to the required service.");
#else // ! MAC_OR_LINUX_
                cerr << "Could not connect to the required service." << endl;
#endif // ! MAC_OR_LINUX_
            }
        }
        else
        {
            OD_LOG("! (stuff->findService(channelNameRequest)"); //####
#if MAC_OR_LINUX_
            GetLogger().fail("Could not find the required service.");
#else // ! MAC_OR_LINUX_
            cerr << "Could not find the required service." << endl;
#endif // ! MAC_OR_LINUX_
        }
        delete stuff;
    }
    else
    {
        OD_LOG("! (stuff)"); //####
    }
    OD_LOG_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief The entry point for communicating with the Address service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Address client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
#if MAC_OR_LINUX_
# pragma unused(argc)
#endif // MAC_OR_LINUX_
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(*argv);
#endif // MAC_OR_LINUX_
    try
    {
        OutputFlavour    flavour;
        YarpStringVector arguments;
        
        if (Utilities::ProcessStandardUtilitiesOptions(argc, argv,
                                                       T_(" ['address' | 'port' | 'both' | tag]"),
                                                       T_("  'address'  Return just the internet "
                                                          "address\n"
                                                          "  'port'     Return just the internet "
                                                          "port\n"
                                                          "  'both'     Return the internet "
                                                          "address and port\n"
                                                          "  tag        The tag for the service to "
                                                          "be connnected to"), 2015,
                                                       STANDARD_COPYRIGHT_NAME, flavour,
                                                       &arguments))
        {
            try
            {
				Utilities::SetUpGlobalStatusReporter();
				Utilities::CheckForNameServerReporter();
                if (Utilities::CheckForValidNetwork())
                {
#if defined(MpM_ReportOnConnections)
					ChannelStatusReporter * reporter = Utilities::GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)
					yarp::os::Network       yarp; // This is necessary to establish any connections
												  // to the YARP infrastructure
                    
                    Initialize(*argv);
                    if (Utilities::CheckForRegistryService())
                    {
#if defined(MpM_ReportOnConnections)
                        setUpAndGo(reporter, arguments, flavour);
#else // ! defined(MpM_ReportOnConnections)
                        setUpAndGo(arguments, flavour);
#endif // ! defined(MpM_ReportOnConnections)
                    }
                    else
                    {
                        OD_LOG("! (Utilities::CheckForRegistryService())"); //####
#if MAC_OR_LINUX_
                        GetLogger().fail("Registry Service not running.");
#else // ! MAC_OR_LINUX_
                        cerr << "Registry Service not running." << endl;
#endif // ! MAC_OR_LINUX_
                    }
                }
                else
                {
                    OD_LOG("! (Utilities::CheckForValidNetwork())"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("YARP network not running.");
#else // ! MAC_OR_LINUX_
                    cerr << "YARP network not running." << endl;
#endif // ! MAC_OR_LINUX_
                }
				Utilities::ShutDownGlobalStatusReporter();
			}
            catch (...)
            {
                OD_LOG("Exception caught"); //####
            }
            yarp::os::Network::fini();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_
