//--------------------------------------------------------------------------------------------------
//
//  File:       m+mServiceMetricsMain.cpp
//
//  Project:    m+m
//
//  Contains:   A utility application to display the metrics for one or more services.
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

#include <m+m/m+mBaseClient.hpp>
#include <m+m/m+mChannelArgumentDescriptor.hpp>
#include <m+m/m+mClientChannel.hpp>
#include <m+m/m+mRequests.hpp>
#include <m+m/m+mServiceRequest.hpp>
#include <m+m/m+mServiceResponse.hpp>
#include <m+m/m+mUtilities.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)

/*! @file
 @brief A utility application to display the metrics for one or more services. */

/*! @dir ServiceMetrics
 @brief The set of files that implement the Service Metrics application. */
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

/*! @brief Set up the environment and perform the operation.
 @param[in] channelName The primary channel for the service.
 @param[in] flavour The format for the output. */
static void
setUpAndGo(const YarpString &  channelName,
           const OutputFlavour flavour)
{
    ODL_ENTER(); //####
    ODL_S1s("channelName = ", channelName); //####
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
    if (Utilities::GetServiceNamesFromCriteria(channelNameRequest, services))
    {
        int matchesCount = static_cast<int>(services.size());

        if (0 < matchesCount)
        {
            bool sawResponse = false;

            if (kOutputFlavourJSON == flavour)
            {
                cout << "[ ";
            }
            for (int ii = 0; ii < matchesCount; ++ii)
            {
                YarpString       aMatch = services[ii];
                yarp::os::Bottle metrics;

                if (Utilities::GetMetricsForService(aMatch, metrics, STANDARD_WAIT_TIME_))
                {
                    YarpString responseAsString(Utilities::ConvertMetricsToString(metrics,
                                                                                  flavour));

                    if (sawResponse)
                    {
                        switch (flavour)
                        {
                            case kOutputFlavourTabs :
                                cout << endl;
                                break;

                            case kOutputFlavourJSON :
                                cout << "," << endl;
                                break;

                            case kOutputFlavourNormal :
                                cout << endl << endl;
                                break;

                            default :
                                break;

                        }
                    }
                    sawResponse = true;
                    if (kOutputFlavourNormal == flavour)
                    {
                        cout << SanitizeString(aMatch, true).c_str() << endl;
                    }
                    cout << responseAsString.c_str();
                }
            }
            if (kOutputFlavourJSON == flavour)
            {
                cout << " ]";
            }
            if (sawResponse)
            {
                cout << endl;
            }
            else
            {
                switch (flavour)
                {
                    case kOutputFlavourJSON :
                    case kOutputFlavourTabs :
                        break;

                    case kOutputFlavourNormal :
                        cout << "No matching service found." << endl;
                        break;

                    default :
                        break;

                }
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

/*! @brief The entry point for displaying service metrics.

 The first, optional, argument is the name of the channel for the service. If the channel is not
 specified, all service channels will be reported. Standard output will receive a list of the
 specified metrics.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the application.
 @return @c 0 on a successful test and @c 1 on failure. */
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
                                                  Utilities::kArgModeOptional, "");
    Utilities::DescriptorVector          argumentList;
    OutputFlavour                        flavour;

    argumentList.push_back(&firstArg);
    if (Utilities::ProcessStandardUtilitiesOptions(argc, argv, argumentList,
                                                   "Display service metrics", 2014,
                                                   STANDARD_COPYRIGHT_NAME_, flavour))
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

                    setUpAndGo(channelName, flavour);
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
    ODL_EXIT_I(0); //####
    return 0;
} // main
