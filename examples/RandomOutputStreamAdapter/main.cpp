//--------------------------------------------------------------------------------------
//
//  File:       RandomOutputStreamAdapter/main.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for an adapter of a simple M+M service.
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
//  Created:    2014-06-25
//
//--------------------------------------------------------------------------------------

#include "M+MAdapterChannel.h"
#include "M+MChannelStatusReporter.h"
#include "M+MRandomOutputStreamAdapterData.h"
#include "M+MRandomOutputStreamClient.h"
#include "M+MRandomOutputStreamInputHandler.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The main application for an adapter of a simple M+M service. */

/*! @dir RandomOutputStreamAdapter
 @brief The set of files that implement an adapter for a simple M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief Run loop control; @c true if the service is to keep going and @c false otherwise. */
static bool lKeepRunning;

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief The signal handler to catch requests to stop the service.
 @param signal The signal being handled. */
static void stopRunning(int signal)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(signal)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_ENTER();//####
    OD_LOG_LL1("signal = ", signal);//####
    lKeepRunning = false;
    OD_LOG_EXIT();//####
} // stopRunning

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for creating an adapter for the example Random Number Service.
 
 The program creates two YARP ports: an output port and an input port. Integers received in the input port are sent to
 the service as the number of random numbers to generate, with zero indicating that the program is to exit.
 The first, optional, argument is the port name to be used for the input port and the second, optional, argument is the
 name to be used for the output port. If the first argument is missing, the input port will be named
 ADAPTER_PORT_NAME_BASE+input/RandomOutputStream and if the second argument is missing, the output port will be named
 ADAPTER_PORT_NAME_BASE+output/RandomOutputStream.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
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
            RandomOutputStreamClient * stuff = new RandomOutputStreamClient;
            
            if (stuff)
            {
                lKeepRunning = true;
                MplusM::Common::SetSignalHandlers(stopRunning);
                if (stuff->findService("keyword randomstream"))
                {
#if defined(MpM_ReportOnConnections)
                    stuff->setReporter(ChannelStatusReporter::gReporter, true);
#endif // defined(MpM_ReportOnConnections)
                    if (stuff->connectToService())
                    {
                        MplusM::Common::AdapterChannel * inputChannel = new MplusM::Common::AdapterChannel(false);
                        MplusM::Common::AdapterChannel * outputChannel = new MplusM::Common::AdapterChannel(true);
                        RandomOutputStreamAdapterData    sharedData(stuff, outputChannel);
                        RandomOutputStreamInputHandler * inputHandler = new RandomOutputStreamInputHandler(sharedData);
                        
                        if (inputChannel && outputChannel && inputHandler)
                        {
                            yarp::os::ConstString inputName(T_(ADAPTER_PORT_NAME_BASE "input/randomoutputstream"));
                            yarp::os::ConstString outputName(T_(ADAPTER_PORT_NAME_BASE "output/randomoutputstream"));
                            
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
                                stuff->addAssociatedChannel(inputChannel);
                                stuff->addAssociatedChannel(outputChannel);
                                sharedData.activate();
                                inputChannel->setReader(*inputHandler);
                                for ( ; lKeepRunning && sharedData.isActive(); )
                                {
#if defined(MpM_MainDoesDelayNotYield)
                                    yarp::os::Time::delay(ONE_SECOND_DELAY);
#else // ! defined(MpM_MainDoesDelayNotYield)
                                    yarp::os::Time::yield();
#endif // ! defined(MpM_MainDoesDelayNotYield)
                                    if (! lKeepRunning)
                                    {
                                        sharedData.deactivate();
                                    }
                                }
                                stuff->removeAssociatedChannels();
                            }
                            else
                            {
                                OD_LOG("! (inputChannel->openWithRetries(inputName, STANDARD_WAIT_TIME) && "
                                       "outputChannel->openWithRetries(outputName, STANDARD_WAIT_TIME))");//####
                                cerr << "Problem opening a channel." << endl;
                            }
#if defined(MpM_DoExplicitClose)
                            inputChannel->close();
                            outputChannel->close();
#endif // defined(MpM_DoExplicitClose)
                        }
                        else
                        {
                            OD_LOG("! (controlChannel && inputChannel && outputChannel && controlHandler && "//####
                                   "inputHandler)");//####
                            cerr << "Problem creating a channel." << endl;
                        }
                        MplusM::Common::AdapterChannel::RelinquishChannel(inputChannel);
                        inputChannel = NULL;
                        MplusM::Common::AdapterChannel::RelinquishChannel(outputChannel);
                        outputChannel = NULL;
                        if (! stuff->disconnectFromService())
                        {
                            OD_LOG("(! stuff->disconnectFromService())");//####
                            cerr << "Problem disconnecting from the service." << endl;
                        }
                    }
                    else
                    {
                        OD_LOG("! (stuff->connectToService())");//####
                        cerr << "Problem connecting to the service." << endl;
                    }
                }
                else
                {
                    OD_LOG("! (stuff->findService(\"keyword randomstream\"))");//####
                    cerr << "Problem locating the service." << endl;
                }
                delete stuff;
            }
            else
            {
                OD_LOG("! (stuff)");//####
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
