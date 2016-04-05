//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRequestInfoMain.cpp
//
//  Project:    m+m
//
//  Contains:   A utility application to list the available requests.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-03-13
//
//--------------------------------------------------------------------------------------------------

#include <m+m/m+mBaseClient.h>
#include <m+m/m+mChannelArgumentDescriptor.h>
#include <m+m/m+mClientChannel.h>
#include <m+m/m+mRequests.h>
#include <m+m/m+mServiceRequest.h>
#include <m+m/m+mServiceResponse.h>
#include <m+m/m+mStringArgumentDescriptor.h>
#include <m+m/m+mUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)

/*! @file
 @brief A utility application to list the available requests. */

/*! @dir RequestInfo
 @brief The set of files that implement the Request Info application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
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

/*! @brief Check the dictionary entry from the 'list' or 'info' response.
 @param asDict The dictionary to be checked.
 @param cleanServiceName The 'sanitized' version of the service name.
 @param flavour The format for the output.
 @param sawResponse @c true if there was already a response output and @c false if this is the
 first.
 @returns @c false if an unexpected value appears and @c true otherwise. */
static bool
processDictionaryEntry(yarp::os::Property & asDict,
                       const YarpString &   cleanServiceName,
                       const OutputFlavour  flavour,
                       const bool           sawResponse)
{
    ODL_ENTER(); //####
    ODL_P1("asDict = ", &asDict); //####
    ODL_S1s("cleanServiceName = ", cleanServiceName); //####
    ODL_B1("sawResponse = ", sawResponse); //####
    bool result = false;

    if (asDict.check(MpM_REQREP_DICT_REQUEST_KEY_))
    {
        YarpString       theDetailsString;
        YarpString       theInputsString;
        YarpString       theOutputsString;
        YarpString       theVersionString;
        YarpString       theRequest(asDict.find(MpM_REQREP_DICT_REQUEST_KEY_).asString());
        yarp::os::Bottle keywordList;

        theRequest = SanitizeString(theRequest, kOutputFlavourJSON != flavour);
        if (asDict.check(MpM_REQREP_DICT_DETAILS_KEY_))
        {
            yarp::os::Value theDetails = asDict.find(MpM_REQREP_DICT_DETAILS_KEY_);

            if (theDetails.isString())
            {
                theDetailsString = theDetails.toString();
            }
        }
        if (asDict.check(MpM_REQREP_DICT_INPUT_KEY_))
        {
            yarp::os::Value theInputs = asDict.find(MpM_REQREP_DICT_INPUT_KEY_);

            if (theInputs.isString())
            {
                theInputsString = theInputs.toString();
            }
        }
        if (asDict.check(MpM_REQREP_DICT_KEYWORDS_KEY_))
        {
            yarp::os::Value theKeywords = asDict.find(MpM_REQREP_DICT_KEYWORDS_KEY_);

            if (theKeywords.isList())
            {
                keywordList = *theKeywords.asList();
            }
        }
        if (asDict.check(MpM_REQREP_DICT_OUTPUT_KEY_))
        {
            yarp::os::Value theOutputs = asDict.find(MpM_REQREP_DICT_OUTPUT_KEY_);

            if (theOutputs.isString())
            {
                theOutputsString = theOutputs.toString();
            }
        }
        if (asDict.check(MpM_REQREP_DICT_VERSION_KEY_))
        {
            yarp::os::Value theVersion = asDict.find(MpM_REQREP_DICT_VERSION_KEY_);

            if (theVersion.isString() || theVersion.isInt() || theVersion.isDouble())
            {
                theVersionString = theVersion.toString();
            }
        }
        switch (flavour)
        {
            case kOutputFlavourTabs :
                cout << cleanServiceName.c_str() << "\t" << theRequest.c_str() << "\t" <<
                        SanitizeString(theVersionString).c_str() << "\t" <<
                        SanitizeString(theDetailsString).c_str() << "\t" <<
                        SanitizeString(keywordList.toString()).c_str() << "\t" <<
                        theInputsString.c_str() << "\t" << theOutputsString.c_str() << endl;
                break;

            case kOutputFlavourJSON :
                if (result || sawResponse)
                {
                    cout << "," << endl;
                }
                cout << T_("{ " CHAR_DOUBLEQUOTE_ "Port" CHAR_DOUBLEQUOTE_ ": "
                           CHAR_DOUBLEQUOTE_) << cleanServiceName.c_str() <<
                        T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "Request" CHAR_DOUBLEQUOTE_ ": "
                           CHAR_DOUBLEQUOTE_) << theRequest.c_str() <<
                        T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "Version" CHAR_DOUBLEQUOTE_ ": "
                           CHAR_DOUBLEQUOTE_) << SanitizeString(theVersionString, true).c_str() <<
                        T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "Details" CHAR_DOUBLEQUOTE_ ": "
                           CHAR_DOUBLEQUOTE_) << SanitizeString(theDetailsString, true).c_str() <<
                        T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "Keywords" CHAR_DOUBLEQUOTE_
                           ": [ ");
                for (int jj = 0, mm = keywordList.size(); mm > jj; ++jj)
                {
                    yarp::os::Value aKeyword(keywordList.get(jj));

                    if (jj)
                    {
                        cout << ", ";
                    }
                    cout << CHAR_DOUBLEQUOTE_ << SanitizeString(aKeyword.toString(), true) <<
                            CHAR_DOUBLEQUOTE_;
                }
                cout << T_(" ], " CHAR_DOUBLEQUOTE_ "Inputs" CHAR_DOUBLEQUOTE_ ": "
                           CHAR_DOUBLEQUOTE_) << theInputsString.c_str() <<
                        T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "Outputs" CHAR_DOUBLEQUOTE_ ": "
                           CHAR_DOUBLEQUOTE_) << theOutputsString.c_str() <<
                        T_(CHAR_DOUBLEQUOTE_ " }");
                break;

            case kOutputFlavourNormal :
                cout << "Service Port: " << cleanServiceName.c_str() << endl;
                cout << "Request:      " << theRequest.c_str() << endl;
                if (0 < theVersionString.length())
                {
                    cout << "Version:      " <<
                    SanitizeString(theVersionString).c_str() << endl;
                }
                if (0 < theDetailsString.length())
                {
                    OutputDescription(cout, "Details:      ", theDetailsString);
                }
                if (0 < keywordList.size())
                {
                    cout << "Keywords:     " <<
                    SanitizeString(keywordList.toString()).c_str() << endl;
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
                break;

            default :
                break;

        }
        result = true;
    }
    ODL_EXIT_B(result); //####
    return result;
} // processDictionary

/*! @brief Process the response to the 'list' or 'info' request sent to a service.
 @param flavour The format for the output.
 @param serviceName The name of the service that generated the response.
 @param response The response to be processed.
 @param sawResponse @c true if there was already a response output and @c false if this is the
 first.
 @returns @c true if some output was generated and @c false otherwise. */
static bool
processResponse(const OutputFlavour     flavour,
                const YarpString &      serviceName,
                const ServiceResponse & response,
                const bool              sawResponse)
{
    ODL_ENTER(); //####
    ODL_S1s("serviceName = ", serviceName); //####
    ODL_P1("response = ", &response); //####
    ODL_B1("sawResponse = ", sawResponse); //####
    bool       result = false;
    YarpString cleanServiceName(SanitizeString(serviceName, kOutputFlavourJSON != flavour));

    for (int ii = 0, howMany = response.count(); ii < howMany; ++ii)
    {
        yarp::os::Value element(response.element(ii));

        if (element.isDict())
        {
            yarp::os::Property * propDict = element.asDict();

            if (propDict)
            {
                result = processDictionaryEntry(*propDict, cleanServiceName, flavour, sawResponse);
            }
        }
        else if (element.isList())
        {
            yarp::os::Bottle * asList = element.asList();

            if (asList)
            {
                yarp::os::Property asDict;

                if (ListIsReallyDictionary(*asList, asDict))
                {
                    result = processDictionaryEntry(asDict, cleanServiceName, flavour, sawResponse);
                }
            }
        }
    }
    ODL_EXIT_B(result); //####
    return result;
} // processResponse

/*! @brief Set up the environment and perform the operation.
 @param channelName The name of the primary channel of the service.
 @param requestName The name of the request being reported.
 @param flavour The format for the output. */
static void
setUpAndGo(const YarpString &  channelName,
           const YarpString &  requestName,
           const OutputFlavour flavour)
{
    ODL_ENTER(); //####
    ODL_S2s("channelName = ", channelName, "requestName = ", requestName); //####
    const char *     requestNameString;
    YarpString       channelNameRequest(MpM_REQREP_DICT_CHANNELNAME_KEY_ ":");
    YarpStringVector services;

    if (0 < channelName.length())
    {
        channelNameRequest += channelName;
    }
    else
    {
        channelNameRequest += "*";
    }
    if (strcmp(requestName.c_str(), "*"))
    {
        requestNameString = requestName.c_str();
    }
    else
    {
        requestNameString = NULL;
    }
    if (Utilities::GetServiceNamesFromCriteria(channelNameRequest, services))
    {
        size_t matchesCount = services.size();

        if (matchesCount)
        {
            YarpString      aName = GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_
                                                         BUILD_NAME_("requestinfo_",
                                                                     DEFAULT_CHANNEL_ROOT_));
            ClientChannel * newChannel = new ClientChannel;

            if (newChannel)
            {
                if (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME_))
                {
                    bool             sawRequestResponse = false;
                    yarp::os::Bottle parameters;

                    if (requestNameString)
                    {
                        parameters.addString(requestNameString);
                    }
                    if (kOutputFlavourJSON == flavour)
                    {
                        cout << "[ ";
                    }
                    for (size_t ii = 0; ii < matchesCount; ++ii)
                    {
                        YarpString aMatch(services[ii]);

                        if (Utilities::NetworkConnectWithRetries(aName, aMatch,
                                                                 STANDARD_WAIT_TIME_))
                        {
                            ServiceResponse response;

                            // If no request was identified, or a wildcard was
                            // specified, we use the 'list' request; otherwise,
                            // do an 'info' request.
                            if (requestNameString)
                            {
                                ServiceRequest request(MpM_INFO_REQUEST_, parameters);

                                if (request.send(*newChannel, response))
                                {
                                    if (0 < response.count())
                                    {
                                        if (processResponse(flavour, aMatch, response,
                                                            sawRequestResponse))
                                        {
                                            sawRequestResponse = true;
                                        }
                                    }
                                }
                                else
                                {
                                    ODL_LOG("! (request.send(*newChannel, response))"); //####
                                    YarpString message("Problem communicating with ");

                                    message += aMatch + ".";
                                    MpM_FAIL_(message.c_str());
                                }
                            }
                            else
                            {
                                ServiceRequest request(MpM_LIST_REQUEST_, parameters);

                                if (request.send(*newChannel, response))
                                {
                                    if (0 < response.count())
                                    {
                                        if (processResponse(flavour, aMatch, response,
                                                            sawRequestResponse))
                                        {
                                            sawRequestResponse = true;
                                        }
                                    }
                                }
                                else
                                {
                                    ODL_LOG("! (request.send(*newChannel, response))"); //####
                                    YarpString message("Problem communicating with ");

                                    message += aMatch + ".";
                                    MpM_FAIL_(message.c_str());
                                }
                            }
#if defined(MpM_DoExplicitDisconnect)
                            if (! Utilities::NetworkDisconnectWithRetries(aName, aMatch,
                                                                          STANDARD_WAIT_TIME_))
                            {
                                ODL_LOG("(! Utilities::NetworkDisconnectWithRetries(aName, " //####
                                       "aMatch, STANDARD_WAIT_TIME_))"); //####
                            }
#endif // defined(MpM_DoExplicitDisconnect)
                        }
                        else
                        {
                            ODL_LOG("! (Utilities::NetworkConnectWithRetries(aName, aMatch, " //####
                                   "STANDARD_WAIT_TIME_))"); //####
                        }
                    }
                    if (kOutputFlavourJSON == flavour)
                    {
                        cout << " ]" << endl;
                    }
                    if (! sawRequestResponse)
                    {
                        switch (flavour)
                        {
                            case kOutputFlavourJSON :
                            case kOutputFlavourTabs :
                                break;

                            case kOutputFlavourNormal :
                                cout << "No matching request found." << endl;
                                break;

                            default :
                                break;

                        }
                    }
#if defined(MpM_DoExplicitClose)
                    newChannel->close();
#endif // defined(MpM_DoExplicitClose)
                }
                else
                {
                    ODL_LOG("! (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME_))"); //####
                }
                delete newChannel;
            }
            else
            {
                ODL_LOG("! (newChannel)"); //####
            }
        }
        else
        {
            switch (flavour)
            {
                case kOutputFlavourJSON :
                case kOutputFlavourTabs :
                    break;

                case kOutputFlavourNormal :
                    cout << "No services found." << endl;
                    break;

                default :
                    break;

            }
        }
    }
    ODL_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for listing the available requests.

 The second, optional, argument is the name of the request to be matched and the first, optional,
 argument is the name of the channel for the service. If the request is not specified, all requests
 will be listed and if the channel is not specified, all service channels will be reported. Standard
 output will receive a list of the specified requests.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the application.
 @returns @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
    YarpString progName(*argv);

    ODL_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport | //####
             kODLoggingOptionWriteToStderr); //####
    ODL_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(progName);
#endif // MAC_OR_LINUX_
    Utilities::ChannelArgumentDescriptor firstArg("channelName", "Channel name for the service",
                                                  Utilities::kArgModeOptional, "*");
    Utilities::StringArgumentDescriptor  secondArg("requestName", "Request name",
                                                   Utilities::kArgModeOptional, "*");
    Utilities::DescriptorVector          argumentList;
    OutputFlavour                        flavour;
    YarpStringVector                     arguments;

    argumentList.push_back(&firstArg);
    argumentList.push_back(&secondArg);
    if (Utilities::ProcessStandardUtilitiesOptions(argc, argv, argumentList,
                                                   "List available requests for services", 2014,
                                                   STANDARD_COPYRIGHT_NAME_, flavour, false,
                                                   &arguments))
    {
        try
        {
            Utilities::SetUpGlobalStatusReporter();
            Utilities::CheckForNameServerReporter();
            if (Utilities::CheckForValidNetwork())
            {
                yarp::os::Network yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure

                Initialize(progName);
                if (Utilities::CheckForRegistryService())
                {
                    YarpString channelName(firstArg.getCurrentValue());
                    YarpString requestName(secondArg.getCurrentValue());

                    setUpAndGo(channelName, requestName, flavour);
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
    ODL_EXIT_L(0); //####
    return 0;
} // main
