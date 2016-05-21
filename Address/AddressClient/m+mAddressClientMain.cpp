//--------------------------------------------------------------------------------------------------
//
//  File:       m+mAddressClientMain.cpp
//
//  Project:    m+m
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

#include "m+mAddressClient.hpp"
#include "m+mAddressRequests.hpp"

#include <m+m/m+mStringArgumentDescriptor.hpp>
#include <m+m/m+mUtilities.hpp>

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

/*! @brief Process the arguments for the application.
 @param[in] outputMode The mode ('both', 'address', 'port') of the output.
 @param[in] tag The tag for the service to be connected to.
 @param[in,out] namePattern The generated search value.
 @param[out] needsAddress @c true if the IP address is requested and @c false otherwise.
 @param[out] needsPort @c true if the port is requested and @c false otherwise.
 @returns @c true if the mode was recognized and @c false otherwise. */
static bool
processArguments(const YarpString & outputMode,
                 const YarpString & tag,
                 YarpString &       namePattern,
                 bool &             needsAddress,
                 bool &             needsPort)
{
    ODL_ENTER(); //####
    ODL_S2s("outputMode = ", outputMode, "tag = ", tag); //####
    ODL_P3("namePattern = ", &namePattern, "needsAddress = ", &needsAddress, //####
           "needsPort = ", &needsPort); //####
    bool okSoFar = true;

    if (0 < outputMode.length())
    {
        if (outputMode == "address")
        {
            needsAddress = true;
            needsPort = false;
        }
        else if (outputMode == "both")
        {
            needsAddress = needsPort = true;
        }
        else if (outputMode == "port")
        {
            needsAddress = false;
            needsPort = true;
        }
        else
        {
            okSoFar = false;
        }
    }
    else
    {
        needsAddress = needsPort = true;
    }
    if (0 < tag.length())
    {
        YarpString singleQuote("'");

        namePattern = singleQuote + namePattern + " " + tag + singleQuote;
    }
    ODL_EXIT_B(okSoFar); //####
    return okSoFar;
} // processArguments

/*! @brief Set up the environment and perform the operation.
 @param[in] outputMode The mode ('both', 'address', 'port') of the output.
 @param[in] tag The tag for the service to be connected to.
 @param[in] flavour The format for the output. */
#if defined(MpM_ReportOnConnections)
static void
setUpAndGo(const YarpString &      outputMode,
           const YarpString &      tag,
           const OutputFlavour     flavour,
           ChannelStatusReporter * reporter)
#else // ! defined(MpM_ReportOnConnections)
static void
setUpAndGo(const YarpString &  outputMode,
           const YarpString &  tag,
           const OutputFlavour flavour)
#endif // ! defined(MpM_ReportOnConnections)
{
    ODL_ENTER(); //####
    ODL_S2s("outputMode = ", outputMode, "tag = ", tag); //####
#if defined(MpM_ReportOnConnections)
    ODL_P1("reporter = ", reporter); //####
#endif // defined(MpM_ReportOnConnections)
    AddressClient * aClient = new AddressClient;

    if (aClient)
    {
        bool       needsAddress;
        bool       needsPort;
        YarpString channelNameRequest(MpM_REQREP_DICT_NAME_KEY_ ":");
        YarpString namePattern(MpM_ADDRESS_CANONICAL_NAME_);

#if defined(MpM_ReportOnConnections)
        aClient->setReporter(*reporter, true);
#endif // defined(MpM_ReportOnConnections)
        if (processArguments(outputMode, tag, namePattern, needsAddress, needsPort))
        {
            channelNameRequest += namePattern;
            if (aClient->findService(channelNameRequest.c_str()))
            {
                if (aClient->connectToService())
                {
                    YarpString address;
                    int        port;

                    if (aClient->getAddress(address, port))
                    {
                        switch (flavour)
                        {
                            case kOutputFlavourJSON :
                                cout << "{ ";
                                if (needsAddress)
                                {
                                    cout << T_(CHAR_DOUBLEQUOTE_ "Address" CHAR_DOUBLEQUOTE_ ": "
                                               CHAR_DOUBLEQUOTE_);
                                    cout << SanitizeString(address).c_str() <<
                                            T_(CHAR_DOUBLEQUOTE_);
                                }
                                if (needsPort)
                                {
                                    if (needsAddress)
                                    {
                                        cout << ", ";
                                    }
                                    cout << T_(CHAR_DOUBLEQUOTE_ "Address" CHAR_DOUBLEQUOTE_ ": "
                                               CHAR_DOUBLEQUOTE_);
                                    cout << port << T_(CHAR_DOUBLEQUOTE_);
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
                        ODL_LOG("! (aClient->getAddress(address, port))"); //####
                        MpM_FAIL_("Problem fetching the address information.");
                    }
                }
                else
                {
                    ODL_LOG("! (aClient->connectToService())"); //####
                    MpM_FAIL_(MSG_COULD_NOT_CONNECT_TO_SERVICE);
                }
            }
            else
            {
                ODL_LOG("! (aClient->findService(channelNameRequest)"); //####
                MpM_FAIL_(MSG_COULD_NOT_FIND_SERVICE);
            }
        }
        else
        {
            cout << "Invalid mode argument." << endl;
        }
        delete aClient;
    }
    else
    {
        ODL_LOG("! (aClient)"); //####
    }
    ODL_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief The entry point for communicating with the Address service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the Address client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
#if MAC_OR_LINUX_
# pragma unused(argc)
#endif // MAC_OR_LINUX_
    YarpString progName(*argv);

    ODL_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport | //####
             kODLoggingOptionWriteToStderr); //####
    ODL_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(progName);
#endif // MAC_OR_LINUX_
    try
    {
        Utilities::StringArgumentDescriptor firstArg("outputMode", T_("The mode of the output "
                                                                      "['address', 'port' or "
                                                                      "'both']"),
                                                     Utilities::kArgModeOptional, "both");
        Utilities::StringArgumentDescriptor secondArg("tag", T_("The tag for the service to be "
                                                                "connnected to"),
                                                      Utilities::kArgModeOptional, "");
        Utilities::DescriptorVector         argumentList;
        OutputFlavour                       flavour;

        argumentList.push_back(&firstArg);
        argumentList.push_back(&secondArg);
        if (Utilities::ProcessStandardClientOptions(argc, argv, argumentList,
                                                    "The client for the Address service", 2015,
                                                    STANDARD_COPYRIGHT_NAME_, flavour))
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

                    Initialize(progName);
                    if (Utilities::CheckForRegistryService())
                    {
                        YarpString outputMode(firstArg.getCurrentValue());
                        YarpString tag(secondArg.getCurrentValue());

#if defined(MpM_ReportOnConnections)
                        setUpAndGo(outputMode, tag, flavour, reporter);
#else // ! defined(MpM_ReportOnConnections)
                        setUpAndGo(outputMode, tag, flavour);
#endif // ! defined(MpM_ReportOnConnections)
                    }
                    else
                    {
                        ODL_LOG("! (Utilities::CheckForRegistryService())"); //####
                        MpM_FAIL_(MSG_REGISTRY_NOT_RUNNING);
                    }
                }
                else
                {
                    ODL_LOG("! (Utilities::CheckForValidNetwork())"); //####
                    MpM_FAIL_(MSG_YARP_NOT_RUNNING);
                }
                Utilities::ShutDownGlobalStatusReporter();
            }
            catch (...)
            {
                ODL_LOG("Exception caught"); //####
            }
            yarp::os::Network::fini();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    ODL_EXIT_L(0); //####
    return 0;
} // main
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_
