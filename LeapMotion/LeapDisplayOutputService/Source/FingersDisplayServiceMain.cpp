//--------------------------------------------------------------------------------------------------
//
//  File:       FingersDisplayServiceMain.cpp
//
//  Project:    m+m
//
//  Contains:   The main application for the fingers display service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2016 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2016-05-09
//
//--------------------------------------------------------------------------------------------------

#include "FingersDisplayService.hpp"

#include <m+m/m+mInputOutputServiceThread.hpp>
#include <m+m/m+mUtilities.hpp>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#include "ofMain.h"
#include "ofApp.h"

using namespace MplusM;
using namespace MplusM::Common;

/*! @brief Set up the environment and start the exemplar output service.
 @param[in] argumentList Descriptions of the arguments to the executable.
 @param[in] progName The path to the executable.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the exemplar output service.
 @param[in] tag The modifier for the service name and port names.
 @param[in] serviceEndpointName The YARP name to be assigned to the new service.
 @param[in] servicePortNumber The port being used by the service.
 @param[in] goWasSet @c true if the service is to be started immediately.
 @param[in] stdinAvailable @c true if running in the foreground and @c false otherwise.
 @param[in] reportOnExit @c true if service metrics are to be reported on exit and @c false otherwise.
 */
static InputOutputServiceThread *
setUpAndGo(const Utilities::DescriptorVector & argumentList,
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
    ODL_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    ODL_S4s("progName = ", progName, "tag = ", tag, "serviceEndpointName = ", //####
            serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    ODL_LL1("argc = ", argc); //####
    ODL_B3("goWasSet = ", goWasSet, "stdinAvailable = ", stdinAvailable, "reportOnExit = ", //####
           reportOnExit); //####
#if 0
    FingersDisplayService *    aService = new FingersDisplayService(argumentList, progName, argc,
                                                                    argv, tag, serviceEndpointName,
                                                                    servicePortNumber);
#endif//0
    InputOutputServiceThread * aThread;

#if 0
    if (NULL == aService)
    {
#endif//0
        ODL_LOG("(NULL == aService)"); //####
        aThread = NULL;
#if 0
    }
    else
    {
        aThread = new InputOutputServiceThread(aService, "", goWasSet, stdinAvailable,
                                               reportOnExit);
        if (NULL == aThread)
        {
            ODL_LOG("(NULL == aThread)"); //####
            delete aService;
        }
        else
        {
            aThread->start();
        }
    }
#endif//0
    ODL_EXIT_P(aThread); //####
    return aThread;
} // setUpAndGo

//========================================================================
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
        AddressTagModifier          modFlag = kModificationNone;
        bool                        goWasSet = false;
        bool                        reportEndpoint = false;
        bool                        reportOnExit = false;
        bool                        stdinAvailable = CanReadFromStandardInput();
        YarpString                  serviceEndpointName;
        YarpString                  servicePortNumber;
        YarpString                  tag;
        Utilities::DescriptorVector argumentList;
        
//#if 0
        if (ProcessStandardServiceOptions(argc, argv, argumentList,
                                          FINGERSDISPLAY_SERVICE_DESCRIPTION_, "", 2016,
                                          STANDARD_COPYRIGHT_NAME_, goWasSet, reportEndpoint,
                                          reportOnExit, tag, serviceEndpointName, servicePortNumber,
                                          modFlag, kSkipNone))
//#endif//0
        {
            Utilities::CheckForNameServerReporter();
            if (Utilities::CheckForValidNetwork())
            {
                yarp::os::Network yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure
                
                Initialize(progName);
                AdjustEndpointName(DEFAULT_FINGERDISPLAY_SERVICE_NAME_, modFlag, tag,
                                   serviceEndpointName);

                if (reportEndpoint)
                {
                    cout << serviceEndpointName.c_str() << endl;
                }
                else if (Utilities::CheckForRegistryService())
                {
                    InputOutputServiceThread * aThread = setUpAndGo(argumentList, progName, argc,
                                                                    argv, tag, serviceEndpointName,
                                                                    servicePortNumber, goWasSet,
                                                                    stdinAvailable, reportOnExit);

                    if (NULL != aThread)
                    {
                        ofSetupOpenGL(1024, 768, OF_WINDOW); // <-------- setup the GL context
                        // this kicks off the running of my app
                        // can be OF_WINDOW or OF_FULLSCREEN
                        // pass in width and height too:
                        ofRunApp(new ofApp);
                        delete aThread;
                    }
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
    ODL_EXIT_L(0); //####
    return 0;
} // main
