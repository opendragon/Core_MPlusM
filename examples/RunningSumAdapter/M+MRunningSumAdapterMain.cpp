//--------------------------------------------------------------------------------------------------
//
//  File:       M+MRunningSumAdapterMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the Running Sum adapter.
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
//  Created:    2014-03-18
//
//--------------------------------------------------------------------------------------------------

#include "M+MRunningSumAdapterData.h"
#include "M+MRunningSumClient.h"
#include "M+MRunningSumControlInputHandler.h"
#include "M+MRunningSumDataInputHandler.h"

#include <mpm/M+MAdapterChannel.h>
#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the Running Sum adapter. */

/*! @dir RunningSumAdapter
 @brief The set of files that implement the Running Sum adapter. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for creating the Running Sum adapter.
 
 The program creates three YARP ports: an output port and two input ports - a control port to
 receive commands and a data port to receive a sequence of numbers to be added. Commands are
 case-sensitive and will result in requests being sent to the service. The commands are:
 
 @c quit Ask the service to stop calculating the running sum and exit from the program.
 
 @c reset Ask the service to reset its running sum.
 
 @c start Ask the service to start calculating the running sum.
 
 @c stop Ask the service to stop calculating the running sum.
 
 The first, optional, argument is the port name to be used for the control port, the second,
 optional, argument is the name to be used for the data port and the third, optional, argument is
 the name to be used for the output port. If the first argument is missing, the control port will be
 named ADAPTER_PORT_NAME_BASE+control/runningsum, if the second argument is missing, the data port
 will be named ADAPTER_PORT_NAME_BASE+data/randomnumber and if the third argument is missing the
 output port will be named ADAPTER_PORT_NAME_BASE+output/runningsum.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the application.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(*argv);
#endif // MAC_OR_LINUX_
    try
    {
        Utilities::SetUpGlobalStatusReporter();
        Utilities::CheckForNameServerReporter();
        if (Utilities::CheckForValidNetwork())
        {
#if defined(MpM_ReportOnConnections)
			ChannelStatusReporter * reporter = Utilities::GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)
			yarp::os::Network       yarp; // This is necessary to establish any connections to the
                                          // YARP infrastructure
            
            Initialize(*argv);
            RunningSumClient * stuff = new RunningSumClient;
            
            if (stuff)
            {
                StartRunning();
                SetSignalHandlers(SignalRunningStop);
                if (stuff->findService("Name: RunningSum"))
                {
#if defined(MpM_ReportOnConnections)
                    stuff->setReporter(*reporter, true);
#endif // defined(MpM_ReportOnConnections)
                    if (stuff->connectToService())
                    {
                        AdapterChannel *                controlChannel = new AdapterChannel(false);
                        AdapterChannel *                dataChannel = new AdapterChannel(false);
                        AdapterChannel *                outputChannel = new AdapterChannel(true);
                        RunningSumAdapterData           sharedData(stuff, outputChannel);
                        RunningSumControlInputHandler * controlHandler =
                                                    new RunningSumControlInputHandler(sharedData);
                        RunningSumDataInputHandler *    dataHandler =
                                                        new RunningSumDataInputHandler(sharedData);
                        
                        if (controlChannel && dataChannel && outputChannel && controlHandler &&
                            dataHandler)
                        {
                            yarp::os::ConstString controlName(T_(ADAPTER_PORT_NAME_BASE
                                                                 "control/runningsum"));
                            yarp::os::ConstString dataName(T_(ADAPTER_PORT_NAME_BASE
                                                              "data/runningsum"));
                            yarp::os::ConstString outputName(T_(ADAPTER_PORT_NAME_BASE
                                                                "output/runningsum"));
                            
                            if (argc > 1)
                            {
                                controlName = argv[1];
                                if (argc > 2)
                                {
                                    dataName = argv[2];
                                    if (argc > 3)
                                    {
                                        outputName = argv[3];
                                    }
                                }
                            }
#if defined(MpM_ReportOnConnections)
                            controlChannel->setReporter(*reporter);
                            controlChannel->getReport(*reporter);
                            dataChannel->setReporter(*reporter);
                            dataChannel->getReport(*reporter);
                            outputChannel->setReporter(*reporter);
                            outputChannel->getReport(*reporter);
#endif // defined(MpM_ReportOnConnections)
                            if (controlChannel->openWithRetries(controlName, STANDARD_WAIT_TIME) &&
                                dataChannel->openWithRetries(dataName, STANDARD_WAIT_TIME) &&
                                outputChannel->openWithRetries(outputName, STANDARD_WAIT_TIME))
                            {
                                double announcedTime = yarp::os::Time::now();
                                
                                stuff->addAssociatedChannel(controlChannel);
                                stuff->addAssociatedChannel(dataChannel);
                                stuff->addAssociatedChannel(outputChannel);
                                sharedData.activate();
                                controlChannel->setReader(*controlHandler);
                                dataChannel->setReader(*dataHandler);
                                for ( ; IsRunning() && sharedData.isActive(); )
                                {
#if defined(MpM_MainDoesDelayNotYield)
                                    yarp::os::Time::delay(ONE_SECOND_DELAY);
#else // ! defined(MpM_MainDoesDelayNotYield)
                                    yarp::os::Time::yield();
#endif // ! defined(MpM_MainDoesDelayNotYield)
                                    if (IsRunning())
                                    {
                                        double now = yarp::os::Time::now();
                                        
                                        if ((announcedTime + ANNOUNCE_INTERVAL) <= now)
                                        {
                                            // Report associated channels again, in case the
                                            // Registry Service has been restarted.
                                            announcedTime = now;
                                            stuff->addAssociatedChannel(controlChannel);
                                            stuff->addAssociatedChannel(dataChannel);
                                            stuff->addAssociatedChannel(outputChannel);
                                        }
                                    }
                                    else
                                    {
                                        sharedData.deactivate();
                                    }
                                }
                                stuff->removeAssociatedChannels();
                            }
                            else
                            {
                                OD_LOG("! (controlChannel->openWithRetries(controlName, " //####
                                       "STANDARD_WAIT_TIME) && " //####
                                       "dataChannel->openWithRetries(dataName, " //####
                                       "STANDARD_WAIT_TIME) && " //####
                                       "outputChannel->openWithRetries(outputName, " //####
                                       "STANDARD_WAIT_TIME))"); //####
#if MAC_OR_LINUX_
                                GetLogger().fail("Problem opening a channel.");
#endif // MAC_OR_LINUX_
                            }
#if defined(MpM_DoExplicitClose)
                            controlChannel->close();
                            dataChannel->close();
                            outputChannel->close();
#endif // defined(MpM_DoExplicitClose)
                        }
                        else
                        {
                            OD_LOG("! (controlChannel && dataChannel && outputChannel && " //####
                                   "controlHandler && dataHandler)"); //####
#if MAC_OR_LINUX_
                            GetLogger().fail("Problem creating a channel.");
#endif // MAC_OR_LINUX_
                        }
                        BaseChannel::RelinquishChannel(controlChannel);
                        BaseChannel::RelinquishChannel(dataChannel);
                        BaseChannel::RelinquishChannel(outputChannel);
                        if (! stuff->disconnectFromService())
                        {
                            OD_LOG("(! stuff->disconnectFromService())"); //####
#if MAC_OR_LINUX_
                            GetLogger().fail("Problem disconnecting from the service.");
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
                    OD_LOG("! (stuff->findService(\"Name: RunningSum\"))"); //####
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
        }
        Utilities::ShutDownGlobalStatusReporter();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
