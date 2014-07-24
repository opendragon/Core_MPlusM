//--------------------------------------------------------------------------------------------------
//
//  File:       RunningSumAltAdapterMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the alternative running sum adapter.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and/or
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
//  Created:    2014-04-15
//
//--------------------------------------------------------------------------------------------------

#include "M+MAdapterChannel.h"
#include "M+MChannelStatusReporter.h"
#include "M+MRunningSumAdapterData.h"
#include "M+MRunningSumClient.h"
#include "M+MRunningSumInputHandler.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The main application for the alternative running sum adapter. */

/*! @dir RunningSumAltAdapter
 @brief The set of files that implement the alternative running sum adapter. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for creating the alternative running sum adapter.
 
 The program creates two YARP ports: an output port and an input port.
 
 a control port to receive commands and a
 data port to receive a sequence of numbers to be added. Commands are case-sensitive and will result
 in requests being sent to the service. The commands are:
 
 quit Ask the service to stop calculating the running sum and exit from the program.
 
 reset Ask the service to reset its running sum.
 
 start Ask the service to start calculating the running sum.
 
 stop Ask the service to stop calculating the running sum.
 
 The first, optional, argument is the port name to be used for the input port and the second,
 optional, argument is the name to be used for the output port. If the first argument is missing,
 the input port will be named ADAPTER_PORT_NAME_BASE+input/runningsum and if the second argument is
 missing, the output port will be named ADAPTER_PORT_NAME_BASE+output/runningsum.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    MplusM::Common::SetUpLogger(*argv);
#endif // MAC_OR_LINUX_
    try
    {
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork())
#endif // CheckNetworkWorks_
        {
            yarp::os::Network yarp; // This is necessary to establish any connections to the YARP
                                    // infrastructure
            
            MplusM::Common::Initialize(*argv);
            RunningSumClient * stuff = new RunningSumClient;
            
            if (stuff)
            {
                MplusM::StartRunning();
                MplusM::Common::SetSignalHandlers(MplusM::SignalRunningStop);
                if (stuff->findService("Name RunningSum", false, NULL, NULL))
                {
#if defined(MpM_ReportOnConnections)
                    stuff->setReporter(ChannelStatusReporter::gReporter, true);
#endif // defined(MpM_ReportOnConnections)
                    if (stuff->connectToService(NULL, NULL))
                    {
                        MplusM::Common::AdapterChannel * inputChannel =
                                                        new MplusM::Common::AdapterChannel(false);
                        MplusM::Common::AdapterChannel * outputChannel =
                                                        new MplusM::Common::AdapterChannel(true);
                        RunningSumAdapterData            sharedData(stuff, outputChannel);
                        RunningSumInputHandler *         inputHandler =
                                                            new RunningSumInputHandler(sharedData);
                        
                        if (inputChannel && outputChannel && inputHandler)
                        {
                            yarp::os::ConstString inputName(T_(ADAPTER_PORT_NAME_BASE
                                                               "input/runningsum"));
                            yarp::os::ConstString outputName(T_(ADAPTER_PORT_NAME_BASE
                                                                "output/runningsum"));
                            if (argc > 1)
                            {
                                inputName = argv[1];
                                if (argc > 2)
                                {
                                    outputName = argv[2];
                                }
                            }
#if defined(MpM_ReportOnConnections)
                            inputChannel->setReporter(ChannelStatusReporter::gReporter);
                            inputChannel->getReport(ChannelStatusReporter::gReporter);
                            outputChannel->setReporter(ChannelStatusReporter::gReporter);
                            outputChannel->getReport(ChannelStatusReporter::gReporter);
#endif // defined(MpM_ReportOnConnections)
                            if (inputChannel->openWithRetries(inputName, STANDARD_WAIT_TIME) &&
                                outputChannel->openWithRetries(outputName, STANDARD_WAIT_TIME))
                            {
                                stuff->addAssociatedChannel(inputChannel, NULL, NULL);
                                stuff->addAssociatedChannel(outputChannel, NULL, NULL);
                                sharedData.activate();
                                inputChannel->setReader(*inputHandler);
                                for ( ; MplusM::IsRunning() && sharedData.isActive(); )
                                {
#if defined(MpM_MainDoesDelayNotYield)
                                    yarp::os::Time::delay(ONE_SECOND_DELAY);
#else // ! defined(MpM_MainDoesDelayNotYield)
                                    yarp::os::Time::yield();
#endif // ! defined(MpM_MainDoesDelayNotYield)
                                    if (! MplusM::IsRunning())
                                    {
                                        sharedData.deactivate();
                                    }
                                }
                                stuff->removeAssociatedChannels(NULL, NULL);
                            }
                            else
                            {
                                OD_LOG("! (inputChannel->openWithRetries(inputName, " //####
                                       "STANDARD_WAIT_TIME) && " //####
                                       "outputChannel->openWithRetries(outputName, " //####
                                       "STANDARD_WAIT_TIME))"); //####
#if MAC_OR_LINUX_
                                MplusM::Common::GetLogger().fail("Problem opening a channel.");
#endif // MAC_OR_LINUX_
                            }
#if defined(MpM_DoExplicitClose)
                            inputChannel->close();
                            outputChannel->close();
#endif // defined(MpM_DoExplicitClose)
                        }
                        else
                        {
                            OD_LOG("! (inputChannel && outputChannel && inputHandler)"); //####
#if MAC_OR_LINUX_
                            MplusM::Common::GetLogger().fail("Problem creating a channel.");
#endif // MAC_OR_LINUX_
                        }
                        MplusM::Common::AdapterChannel::RelinquishChannel(inputChannel);
                        MplusM::Common::AdapterChannel::RelinquishChannel(outputChannel);
                        if (! stuff->disconnectFromService(NULL, NULL))
                        {
                            OD_LOG("(! stuff->disconnectFromService(NULL, NULL))"); //####
#if MAC_OR_LINUX_
                            MplusM::Common::GetLogger().fail("Problem disconnecting from the "
                                                             "service.");
#endif // MAC_OR_LINUX_
                        }
                    }
                    else
                    {
                        OD_LOG("! (stuff->connectToService(NULL, NULL))"); //####
#if MAC_OR_LINUX_
                        MplusM::Common::GetLogger().fail("Problem connecting to the service.");
#endif // MAC_OR_LINUX_
                    }
                }
                else
                {
                    OD_LOG("! (stuff->findService(\"Name RunningSum\", false, NULL, NULL))"); //####
#if MAC_OR_LINUX_
                    MplusM::Common::GetLogger().fail("Problem locating the service.");
#endif // MAC_OR_LINUX_
                }
                delete stuff;
            }
            else
            {
                OD_LOG("! (stuff)"); //####
            }
        }
#if CheckNetworkWorks_
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork())"); //####
# if MAC_OR_LINUX_
            MplusM::Common::GetLogger().fail("YARP network not running.");
# endif // MAC_OR_LINUX_
        }
#endif // CheckNetworkWorks_
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
