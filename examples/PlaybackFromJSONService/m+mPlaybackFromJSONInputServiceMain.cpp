//--------------------------------------------------------------------------------------------------
//
//  File:       m+mPlaybackFromJSONInputServiceMain.cpp
//
//  Project:    m+m
//
//  Contains:   The main application for the Playback From JSON input service.
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
//  Created:    2015-08-24
//
//--------------------------------------------------------------------------------------------------

#include "m+mPlaybackFromJSONInputService.hpp"

#include <m+m/m+mBoolArgumentDescriptor.hpp>
#include <m+m/m+mDoubleArgumentDescriptor.hpp>
#include <m+m/m+mEndpoint.hpp>
#include <m+m/m+mFilePathArgumentDescriptor.hpp>
#include <m+m/m+mUtilities.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the Playback From JSON input service. */

/*! @dir PlaybackFromJSONService
 @brief The set of files that implement the Playback From JSON input service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;
using std::cerr;
using std::cin;
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

/*! @brief Set up the environment and start the Playback From JSON input service.
 @param[in] inputPath The path to the data file.
 @param[in] argumentList Descriptions of the arguments to the executable.
 @param[in] progName The path to the executable.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the Playback From JSON input service.
 @param[in] tag The modifier for the service name and port names.
 @param[in] serviceEndpointName The YARP name to be assigned to the new service.
 @param[in] servicePortNumber The port being used by the service.
 @param[in] goWasSet @c true if the service is to be started immediately.
 @param[in] stdinAvailable @c true if running in the foreground and @c false otherwise.
 @param[in] reportOnExit @c true if service metrics are to be reported on exit and @c false
 otherwise. */
static void
setUpAndGo(const YarpString &                  inputPath,
           const Utilities::DescriptorVector & argumentList,
           const YarpString &                  progName,
           const int                           argc,
           char * *                            argv,
           const YarpString &                  tag,
           const YarpString &                  serviceEndpointName,
           const YarpString &                  servicePortNumber,
           const bool                          goWasSet,
           const bool                          stdinAvailable,
           const bool                          reportOnExit)
{
    ODL_ENTER(); //####
    ODL_S4s("inputPath = ", inputPath, "progName = ", progName, "tag = ", tag, //####
            "serviceEndpointName = ", serviceEndpointName); //####
    ODL_S1s("servicePortNumber = ", servicePortNumber); //####
    ODL_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    ODL_I1("argc = ", argc); //####
    ODL_B3("goWasSet = ", goWasSet, "stdinAvailable = ", stdinAvailable, "reportOnExit = ", //####
           reportOnExit); //####
    PlaybackFromJSONInputService * aService = new PlaybackFromJSONInputService(inputPath,
                                                                               argumentList,
                                                                               progName, argc, argv,
                                                                               tag,
                                                                               serviceEndpointName,
                                                                               servicePortNumber);

    if (aService)
    {
        aService->performLaunch("", goWasSet, stdinAvailable, reportOnExit);
        delete aService;
    }
    else
    {
        ODL_LOG("! (aService)"); //####
    }
    ODL_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the Playback From JSON input service.

 The second, optional, argument is the port number to be used and the first, optional, argument is
 the name of the channel to be used. There is no output.
 The option 'p' specifies the burst period, in seconds, while the option 's' specifies the number of
 random values to generate in each burst.
 The option 'r' indicates that the service metrics are to be reported on exit.
 The option 't' specifies the tag modifier, which is applied to the name of the channel, if the
 name was not specified. It is also applied to the service name as a suffix.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the Playback From JSON input service.
 @return @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
    YarpString progName(*argv);

#if defined(MpM_ServicesLogToStandardError)
    ODL_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionWriteToStderr | //####
             kODLoggingOptionEnableThreadSupport); //####
#else // ! defined(MpM_ServicesLogToStandardError)
    ODL_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport); //####
#endif // ! defined(MpM_ServicesLogToStandardError)
    ODL_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(progName);
#endif // MAC_OR_LINUX_
    try
    {
        AddressTagModifier                    modFlag = kModificationNone;
        bool                                  goWasSet = false;
        bool                                  reportEndpoint = false;
        bool                                  reportOnExit = false;
        bool                                  stdinAvailable = CanReadFromStandardInput();
        YarpString                            serviceEndpointName;
        YarpString                            servicePortNumber;
        YarpString                            tag;
        Utilities::FilePathArgumentDescriptor firstArg("filePath", T_("Path to input file"),
                                                       Utilities::kArgModeRequired, "", "", false,
                                                       false);
        Utilities::DoubleArgumentDescriptor   secondArg("ratio", T_("Playback ratio"),
                                                        Utilities::kArgModeOptionalModifiable, 1,
                                                        true, 0, false, 0);
        Utilities::DoubleArgumentDescriptor   thirdArg("initialDelay", T_("Initial delay"),
                                                        Utilities::kArgModeOptionalModifiable, 0,
                                                        true, 0, false, 0);
        Utilities::BoolArgumentDescriptor     fourthArg("loop", T_("Loop the playback"),
                                                        Utilities::kArgModeOptionalModifiable,
                                                        true);
        Utilities::DescriptorVector           argumentList;

        argumentList.push_back(&firstArg);
        argumentList.push_back(&secondArg);
        argumentList.push_back(&thirdArg);
        argumentList.push_back(&fourthArg);
        if (ProcessStandardServiceOptions(argc, argv, argumentList,
                                          PLAYBACKFROMJSONINPUT_SERVICE_DESCRIPTION_, "", 2015,
                                          STANDARD_COPYRIGHT_NAME_, goWasSet, reportEndpoint,
                                          reportOnExit, tag, serviceEndpointName, servicePortNumber,
                                          modFlag, kSkipNone))
        {
            Utilities::CheckForNameServerReporter();
            if (Utilities::CheckForValidNetwork())
            {
                yarp::os::Network yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure

                Initialize(progName);
                YarpString inputPath(firstArg.getCurrentValue());
                YarpString tagModifier =
                                Utilities::GetFileNameBase(Utilities::GetFileNamePart(inputPath));

                AdjustEndpointName(DEFAULT_PLAYBACKFROMJSONINPUT_SERVICE_NAME_, modFlag, tag,
                                   serviceEndpointName, tagModifier);
                if (reportEndpoint)
                {
                    cout << serviceEndpointName.c_str() << endl;
                }
                else if (Utilities::CheckForRegistryService())
                {
                    setUpAndGo(inputPath, argumentList, progName, argc, argv, tag,
                               serviceEndpointName, servicePortNumber, goWasSet, stdinAvailable,
                               reportOnExit);
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
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    ODL_EXIT_I(0); //####
    return 0;
} // main
