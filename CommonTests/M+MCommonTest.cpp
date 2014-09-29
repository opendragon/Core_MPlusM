//--------------------------------------------------------------------------------------------------
//
//  File:       CommonTests/M+MCommonTest.cpp
//
//  Project:    M+M
//
//  Contains:   The test driver for the unit tests of the M+M common library.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#include "M+MTest03Handler.h"
#include "M+MTest04Handler.h"
#include "M+MTest05HandlerCreator.h"
#include "M+MTest08Handler.h"
#include "M+MTest09Service.h"
#include "M+MTest10Service.h"
#include "M+MTest11Service.h"
#include "M+MTest12Service.h"

#include <mpm/M+MClientChannel.h>
#include <mpm/M+MEndpoint.h>
#include <mpm/M+MRequests.h>
#include <mpm/M+MServiceRequest.h>
#include <mpm/M+MServiceResponse.h>
#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The test driver for the unit tests of the M+M common library. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Test;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Create an endpoint for a test.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the endpoint constructor.
 @returns A newly created endpoint, or @c NULL if one could not be created. */
static Endpoint * doCreateEndpointForTest(const int argc,
                                          char * *  argv)
{
    OD_LOG_ENTER(); //####
    Endpoint * stuff = NULL;
    
    try
    {
        if (0 < argc)
        {
            switch (argc)
            {
                    // Argument order for tests = endpoint name [, port]
                case 1 :
                    stuff = new Endpoint(*argv);
                    break;
                    
                case 2 :
                    stuff = new Endpoint(*argv, argv[1]);
                    break;
                    
                default :
                    break;
                    
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_P(stuff); //####
    return stuff;
} // doCreateEndpointForTest

/*! @brief Create a temporary channel for a test.
 @param destinationName The name of the channel to be connected to.
 @param channelPath The root path for the new temporary channel.
 @returns A pointer to a newly-allocated temporary channel. */
static ClientChannel * doCreateTestChannel(const yarp::os::ConstString & destinationName,
                                           const char *                  channelPath)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2("destinationName = ", destinationName.c_str(), "channelPath = ", channelPath); //####
    yarp::os::ConstString   aName(GetRandomChannelName(channelPath));
    ClientChannel *         newChannel = new ClientChannel;
#if defined(MpM_ReportOnConnections)
    ChannelStatusReporter & reporter = *Utilities::GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)
    
    
    if (newChannel)
    {
#if defined(MpM_ReportOnConnections)
        newChannel->setReporter(reporter);
        newChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
        if (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
        {
            if (! Utilities::NetworkConnectWithRetries(aName, destinationName, STANDARD_WAIT_TIME,
                                                       false, NULL, NULL))
            {
                OD_LOG("(! Utilities::NetworkConnectWithRetries(aName, destinationName, " //####
                       "STANDARD_WAIT_TIME, false, NULL, NULL))"); //####
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
                ClientChannel::RelinquishChannel(newChannel);
                newChannel = NULL;
            }
        }
        else
        {
            OD_LOG("! (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))"); //####
        }
    }
    else
    {
        OD_LOG("! (newChannel)"); //####
    }
    OD_LOG_EXIT_P(newChannel); //####
    return newChannel;
} // doCreateTestChannel

/*! @brief Create a temporary channel for a test.
 @param anEndpoint The endpoint to be connected to.
 @param channelPath The root path for the new temporary channel.
 @returns A pointer to a newly-allocated temporary channel. */
static ClientChannel * doCreateTestChannel(Endpoint &   anEndpoint,
                                           const char * channelPath)
{
    return doCreateTestChannel(anEndpoint.getName(), channelPath);
} // doCreateTestChannel

/*! @brief Destroy a temporary channel that was used with a test.
 @param destinationName The name of the channel that the temporary channel was connected to.
 @param theChannel A pointer to the temporary channel. */
static void doDestroyTestChannel(const yarp::os::ConstString & destinationName,
                                 ClientChannel *               theChannel)
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(destinationName)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    OD_LOG_ENTER(); //####
    OD_LOG_P1("theChannel = ", theChannel); //####
    if (theChannel)
    {
#if defined(MpM_DoExplicitDisconnect)
        if (! Utilities::NetworkDisconnectWithRetries(theChannel->name(), destinationName,
                                                      STANDARD_WAIT_TIME, NULL, NULL))
        {
            OD_LOG("(! Utilities::NetworkDisconnectWithRetries(theChannel->name(), " //####
                   "destinationName, STANDARD_WAIT_TIME, NULL, NULL))"); //####
        }
#endif // defined(MpM_DoExplicitDisconnect)
#if defined(MpM_DoExplicitClose)
        theChannel->close();
#endif // defined(MpM_DoExplicitClose)
        ClientChannel::RelinquishChannel(theChannel);
    }
    OD_LOG_EXIT(); //####
} // doDestroyTestChannel

/*! @brief Destroy a temporary channel that was used with a test.
 @param anEndpoint The endpoint to be connected to.
 @param theChannel A pointer to the temporary channel. */
static void doDestroyTestChannel(Endpoint &      anEndpoint,
                                 ClientChannel * theChannel)
{
    doDestroyTestChannel(anEndpoint.getName(), theChannel);
} // doDestroyTestChannel

#if defined(__APPLE__)
# pragma mark *** Test Case 01 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestCreateEndpoint(const char * launchPath,
                                const int    argc,
                                char * *     argv) // create endpoint
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        Endpoint * stuff = doCreateEndpointForTest(argc, argv);
        
        if (stuff)
        {
            if (stuff->open(STANDARD_WAIT_TIME))
            {
                OD_LOG_S1s("endpoint name = ", stuff->getName());
                result = 0;
            }
            else
            {
                OD_LOG("! (stuff->open(STANDARD_WAIT_TIME))"); //####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestCreateEndpoint

#if defined(__APPLE__)
# pragma mark *** Test Case 02 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestConnectToEndpoint(const char * launchPath,
                                   const int    argc,
                                   char * *     argv) // connect to endpoint
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        Endpoint *              stuff = doCreateEndpointForTest(argc, argv);
        ChannelStatusReporter & reporter = *Utilities::GetGlobalStatusReporter();
        
        if (stuff)
        {
            if (stuff->open(STANDARD_WAIT_TIME) && stuff->setReporter(reporter, true))
            {
                OD_LOG_S1s("endpoint name = ", stuff->getName());
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomChannelName("_test_/connecttoendpoint_"));
                ClientChannel *       outChannel = new ClientChannel;
                
                if (outChannel)
                {
#if defined(MpM_ReportOnConnections)
                    outChannel->setReporter(reporter);
                    outChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
                    if (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
                    {
                        outChannel->getReport(reporter);
                        OD_LOG_S1s("endpoint name = ", stuff->getName());
                        if (outChannel->addOutputWithRetries(stuff->getName(), STANDARD_WAIT_TIME))
                        {
                            result = 0;
#if defined(MpM_DoExplicitDisconnect)
                            if (! NetworkDisconnectWithRetries(outChannel->name(), stuff->getName(),
                                                               STANDARD_WAIT_TIME, NULL, NULL))
                            {
                                OD_LOG("(! NetworkDisconnectWithRetries(outChannel->name(), " //####
                                       "stuff->getName(), STANDARD_WAIT_TIME, NULL, NULL))"); //####
                            }
#endif // defined(MpM_DoExplicitDisconnect)
                        }
                        else
                        {
                            OD_LOG("! (outChannel->addOutputWithRetries(stuff->getName(), " //####
                                   "STANDARD_WAIT_TIME))"); //####
                        }
#if defined(MpM_DoExplicitClose)
                        outChannel->close();
#endif // defined(MpM_DoExplicitClose)
                    }
                    else
                    {
                        OD_LOG("! (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME))"); //####
                    }
                    ClientChannel::RelinquishChannel(outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");
                }
            }
            else
            {
                OD_LOG("! (stuff->open(STANDARD_WAIT_TIME) && " //####
                       "stuff->setReporter(reporter, true))"); //####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestConnectToEndpoint

#if defined(__APPLE__)
# pragma mark *** Test Case 03 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestWriteToEndpoint(const char * launchPath,
                                 const int    argc,
                                 char * *     argv) // send to endpoint
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        Endpoint *              stuff = doCreateEndpointForTest(argc, argv);
        ChannelStatusReporter & reporter = *Utilities::GetGlobalStatusReporter();
        
        if (stuff)
        {
            Test03Handler handler;
            
            if (stuff->setInputHandler(handler) && stuff->open(STANDARD_WAIT_TIME) &&
                stuff->setReporter(reporter, true))
            {
                OD_LOG_S1s("endpoint name = ", stuff->getName());
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomChannelName("_test_/writetoendpoint_"));
                ClientChannel *       outChannel = new ClientChannel;
                
                if (outChannel)
                {
#if defined(MpM_ReportOnConnections)
                    outChannel->setReporter(reporter);
                    outChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
                    if (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
                    {
                        outChannel->getReport(reporter);
                        if (outChannel->addOutputWithRetries(stuff->getName(), STANDARD_WAIT_TIME))
                        {
                            yarp::os::Bottle message;
#if defined(MpM_ChannelsUseRpc)
                            yarp::os::Bottle response;
#endif // defined(MpM_ChannelsUseRpc)
                            
                            message.addString(aName);
                            message.addString("howdi");
#if defined(MpM_ChannelsUseRpc)
                            if (outChannel->write(message, response))
                            {
                                result = 0;
# if defined(MpM_DoExplicitDisconnect)
                                if (! NetworkDisconnectWithRetries(outChannel->name(),
                                                                   stuff->getName(),
                                                                   STANDARD_WAIT_TIME, NULL, NULL))
                                {
                                    OD_LOG("(! NetworkDisconnectWithRetries(outChannel->" //####
                                           "name(), stuff->getName(), STANDARD_WAIT_TIME, " //####
                                           "NULL, NULL))"); //####
                                }
# endif // defined(MpM_DoExplicitDisconnect)
                            }
#else // ! defined(MpM_ChannelsUseRpc)
                            if (outChannel->write(message))
                            {
                                result = 0;
# if defined(MpM_DoExplicitDisconnect)
                                if (! NetworkDisconnectWithRetries(outChannel->name(),
                                                                   stuff->getName(),
                                                                   STANDARD_WAIT_TIME, NULL, NULL))
                                {
                                    OD_LOG("(! NetworkDisconnectWithRetries(outChannel->" //####
                                           "name(), stuff->getName(), STANDARD_WAIT_TIME, " //####
                                           "NULL, NULL))"); //####
                                }
# endif // defined(MpM_DoExplicitDisconnect)
                            }
#endif // ! defined(MpM_ChannelsUseRpc)
                            else
                            {
                                OD_LOG("! (outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                                Stall();
#endif // defined(MpM_StallOnSendProblem)
                            }
                        }
                        else
                        {
                            OD_LOG("! (outChannel->addOutputWithRetries(stuff->getName(), " //####
                                   "STANDARD_WAIT_TIME))"); //####
                        }
#if defined(MpM_DoExplicitClose)
                        outChannel->close();
#endif // defined(MpM_DoExplicitClose)
                    }
                    else
                    {
                        OD_LOG("! (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME))"); //####
                    }
                    ClientChannel::RelinquishChannel(outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");
                }
            }
            else
            {
                OD_LOG("! (stuff->setInputHandler(handler) && " //####
                       "stuff->open(STANDARD_WAIT_TIME) && " //####
                       "stuff->setReporter(reporter, true))"); //####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestWriteToEndpoint

#if defined(__APPLE__)
# pragma mark *** Test Case 04 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestEchoFromEndpointWithReader(const char * launchPath,
                                            const int    argc,
                                            char * *     argv) // send to endpoint
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        Endpoint *              stuff = doCreateEndpointForTest(argc, argv);
        ChannelStatusReporter & reporter = *Utilities::GetGlobalStatusReporter();
        
        if (stuff)
        {
            Test04Handler handler;
            
            if (stuff->setInputHandler(handler) && stuff->open(STANDARD_WAIT_TIME) &&
                stuff->setReporter(reporter, true))
            {
                OD_LOG_S1s("endpoint name = ", stuff->getName());
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomChannelName("_test_/echofromendpoint"
                                                                 "withreader_"));
                ClientChannel *       outChannel = new ClientChannel;
                
                if (outChannel)
                {
#if defined(MpM_ReportOnConnections)
                    outChannel->setReporter(reporter);
                    outChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
                    if (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
                    {
                        outChannel->getReport(reporter);
                        if (outChannel->addOutputWithRetries(stuff->getName(), STANDARD_WAIT_TIME))
                        {
                            yarp::os::Bottle message;
                            yarp::os::Bottle response;
                            
                            message.addString(aName);
                            message.addString("howdi");
                            if (outChannel->write(message, response))
                            {
                                result = 0;
#if defined(MpM_DoExplicitDisconnect)
                                if (! NetworkDisconnectWithRetries(outChannel->name(),
                                                                   stuff->getName(),
                                                                   STANDARD_WAIT_TIME, NULL, NULL))
                                {
                                    OD_LOG("(! NetworkDisconnectWithRetries(outChannel->" //####
                                           "name(), stuff->getName(), STANDARD_WAIT_TIME, " //####
                                           "NULL, NULL))"); //####
                                }
#endif // defined(MpM_DoExplicitDisconnect)
                            }
                            else
                            {
                                OD_LOG("! (outChannel->write(message, response))"); //####
#if defined(MpM_StallOnSendProblem)
                                Stall();
#endif // defined(MpM_StallOnSendProblem)
                            }
                        }
                        else
                        {
                            OD_LOG("! (outChannel->addOutputWithRetries(stuff->getName(), " //####
                                   "STANDARD_WAIT_TIME))"); //####
                        }
#if defined(MpM_DoExplicitClose)
                        outChannel->close();
#endif // defined(MpM_DoExplicitClose)
                    }
                    else
                    {
                        OD_LOG("! (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME))"); //####
                    }
                    ClientChannel::RelinquishChannel(outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");
                }
            }
            else
            {
                OD_LOG("! (stuff->setInputHandler(handler) && " //####
                       "stuff->open(STANDARD_WAIT_TIME) && " //####
                       "stuff->setReporter(reporter, true))"); //####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestEchoFromEndpointWithReader

#if defined(__APPLE__)
# pragma mark *** Test Case 05 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestEchoFromEndpointWithReaderCreator(const char * launchPath,
                                                   const int    argc,
                                                   char * *     argv) // send to endpoint
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        Endpoint *              stuff = doCreateEndpointForTest(argc, argv);
        ChannelStatusReporter & reporter = *Utilities::GetGlobalStatusReporter();
        
        if (stuff)
        {
            Test05HandlerCreator handlerCreator;
            
            if (stuff->setInputHandlerCreator(handlerCreator) && stuff->open(STANDARD_WAIT_TIME) &&
                stuff->setReporter(reporter, true))
            {
                OD_LOG_S1s("endpoint name = ", stuff->getName());
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomChannelName("_test_/echofromendpoint"
                                                                 "withreadercreator_"));
                ClientChannel *       outChannel = new ClientChannel;
                
                if (outChannel)
                {
#if defined(MpM_ReportOnConnections)
                    outChannel->setReporter(reporter);
                    outChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
                    if (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
                    {
                        outChannel->getReport(reporter);
                        if (outChannel->addOutputWithRetries(stuff->getName(), STANDARD_WAIT_TIME))
                        {
                            yarp::os::Bottle message;
                            yarp::os::Bottle response;
                            
                            message.addString(aName);
                            message.addString("howdi");
                            if (outChannel->write(message, response))
                            {
                                result = 0;
#if defined(MpM_DoExplicitDisconnect)
                                if (! NetworkDisconnectWithRetries(outChannel->name(),
                                                                   stuff->getName(),
                                                                   STANDARD_WAIT_TIME, NULL, NULL))
                                {
                                    OD_LOG("(! NetworkDisconnectWithRetries(outChannel->" //####
                                           "name(), stuff->getName(), STANDARD_WAIT_TIME, " //####
                                           "NULL, NULL))"); //####
                                }
#endif // defined(MpM_DoExplicitDisconnect)
                            }
                            else
                            {
                                OD_LOG("! (outChannel->write(message, response))"); //####
#if defined(MpM_StallOnSendProblem)
                                Stall();
#endif // defined(MpM_StallOnSendProblem)
                            }
                        }
                        else
                        {
                            OD_LOG("! (outChannel->addOutputWithRetries(stuff->getName(), " //####
                                   "STANDARD_WAIT_TIME))"); //####
                        }
#if defined(MpM_DoExplicitClose)
                        outChannel->close();
#endif // defined(MpM_DoExplicitClose)
                    }
                    else
                    {
                        OD_LOG("! (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME))"); //####
                    }
                    ClientChannel::RelinquishChannel(outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");
                }
            }
            else
            {
                OD_LOG("! (stuff->setInputHandlerCreator(handlerCreator) && " //####
                       "stuff->open(STANDARD_WAIT_TIME) && " //####
                       "stuff->setReporter(&reporter, true))"); //####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestEchoFromEndpointWithReaderCreator

#if defined(__APPLE__)
# pragma mark *** Test Case 06 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestCreateRequest(const char * launchPath,
                               const int    argc,
                               char * *     argv) // create request
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        if (0 == argc)
        {
            OD_LOG("0 == argc"); //####
        }
        else
        {
            yarp::os::Bottle parameters;
            
            for (int ii = 1; ii < argc; ++ii)
            {
                parameters.addString(argv[ii]);
            }
            ServiceRequest * stuff = new ServiceRequest(*argv, parameters);
            
            delete stuff;
            result = 0;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestCreateRequest

#if defined(__APPLE__)
# pragma mark *** Test Case 07 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestCreateResponse(const char * launchPath,
                                const int    argc,
                                char * *     argv) // create request
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        yarp::os::Bottle parameters;
        
        for (int ii = 0; ii < argc; ++ii)
        {
            parameters.addString(argv[ii]);
        }
        ServiceResponse * stuff = new ServiceResponse(parameters);
        
        delete stuff;
        result = 0;
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestCreateResponse

#if defined(__APPLE__)
# pragma mark *** Test Case 08 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestRequestEchoFromEndpoint(const char * launchPath,
                                         const int    argc,
                                         char * *     argv) // create request
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        Endpoint *              stuff = doCreateEndpointForTest(argc, argv);
        ChannelStatusReporter & reporter = *Utilities::GetGlobalStatusReporter();
        
        if (stuff)
        {
            Test08Handler handler;
            
            if (stuff->setInputHandler(handler) && stuff->open(STANDARD_WAIT_TIME) &&
                stuff->setReporter(reporter, true))
            {
                ClientChannel * outChannel = doCreateTestChannel(stuff->getName(),
                                                                 "test/requestechofromendpoint_");
                
                if (outChannel)
                {
                    OD_LOG_S1s("endpoint name = ", stuff->getName());
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(MpM_ECHO_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*outChannel, &response))
                    {
                        OD_LOG_LL1("response size = ", response.count()); //####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_LOG_S1s("response value = ", response.element(ii).toString()); //####
                        }
                        result = 0;
                    }
                    else
                    {
                        OD_LOG("! (request.send(*outChannel, &response))"); //####
                    }
                    doDestroyTestChannel(stuff->getName(), outChannel);
                    outChannel = NULL;
                }
                else
                {
                    OD_LOG("! (outChannel)"); //####
                }
            }
            else
            {
                OD_LOG("! (stuff->setInputHandler(handler) && " //####
                       "stuff->open(STANDARD_WAIT_TIME) && " //####
                       "stuff->setReporter(reporter, true))"); //####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestRequestEchoFromEndpoint

#if defined(__APPLE__)
# pragma mark *** Test Case 09 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestRequestEchoFromServiceUsingDefaultWithReader(const char * launchPath,
                                                              const int    argc,
                                                              char * *     argv) // send 'echo'
                                                                                 // request
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        Test09Service * stuff = new Test09Service(launchPath, argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                ClientChannel * outChannel = doCreateTestChannel(stuff->getEndpoint(),
                                                                 "test/requestechofromservice"
                                                                 "usingdefaultwithreader");
                
                if (outChannel)
                {
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(MpM_ECHO_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*outChannel, &response))
                    {
                        OD_LOG_LL1("response size = ", response.count()); //####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_LOG_S1s("response value = ", response.element(ii).toString()); //####
                        }
                        result = 0;
                    }
                    else
                    {
                        OD_LOG("! (request.send(*outChannel, &response))"); //####
                    }
                    doDestroyTestChannel(stuff->getEndpoint(), outChannel);
                    outChannel = NULL;
                }
                else
                {
                    OD_LOG("! (outChannel)"); //####
                }
                stuff->stop();
            }
            else
            {
                OD_LOG("! (stuff->start())"); //####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestRequestEchoFromServiceUsingDefaultWithReader

#if defined(__APPLE__)
# pragma mark *** Test Case 10 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestRequestEchoFromServiceUsingDefaultWithReaderCreator(const char * launchPath,
                                                                     const int    argc,
                                                                     char * *     argv) // send
                                                                                        // 'echo'
                                                                                        // request
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        Test10Service * stuff = new Test10Service(launchPath, argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                ClientChannel * outChannel = doCreateTestChannel(stuff->getEndpoint(),
                                                                 "test/requestechofromservice"
                                                                 "usingdefaultwithreadercreator_");
                
                if (outChannel)
                {
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(MpM_ECHO_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*outChannel, &response))
                    {
                        OD_LOG_LL1("response size = ", response.count()); //####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_LOG_S1s("response value = ", response.element(ii).toString()); //####
                        }
                        result = 0;
                    }
                    else
                    {
                        OD_LOG("! (request.send(*outChannel, &response))"); //####
                    }
                    doDestroyTestChannel(stuff->getEndpoint(), outChannel);
                    outChannel = NULL;
                }
                else
                {
                    OD_LOG("! (outChannel)"); //####
                }
                stuff->stop();
            }
            else
            {
                OD_LOG("! (stuff->start())"); //####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestRequestEchoFromServiceUsingDefaultWithReaderCreator

#if defined(__APPLE__)
# pragma mark *** Test Case 11 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestRequestEchoFromServiceWithRequestHandler(const char * launchPath,
                                                          const int    argc,
                                                          char * *     argv) // create 'echo'
                                                                             // request
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        Test11Service * stuff = new Test11Service(launchPath, argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                ClientChannel * outChannel = doCreateTestChannel(stuff->getEndpoint(),
                                                                 "test/requestechofromservice"
                                                                 "withrequesthandler_");
                
                if (outChannel)
                {
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(MpM_ECHO_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*outChannel, &response))
                    {
                        if (3 == response.count())
                        {
                            yarp::os::ConstString expected[] =
                            {
                                "some", "to", "send"
                            };
                            
                            result = 0;
                            for (int ii = 0; (! result) && (ii < response.count()); ++ii)
                            {
                                if (expected[ii] != response.element(ii).toString())
                                {
                                    OD_LOG_S2s("expected[ii] = ", expected[ii], //####
                                               "response.element(ii).toString() = ", //####
                                               response.element(ii).toString()); //####
                                    result = 1;
                                }
                            }
                        }
                        else
                        {
                            OD_LOG("! (3 == response.count())"); //####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request.send(*outChannel, &response))"); //####
                    }
                    doDestroyTestChannel(stuff->getEndpoint(), outChannel);
                    outChannel = NULL;
                }
                else
                {
                    OD_LOG("! (outChannel)"); //####
                }
                stuff->stop();
            }
            else
            {
                OD_LOG("! (stuff->start())"); //####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestRequestEchoFromServiceWithRequestHandler

#if defined(__APPLE__)
# pragma mark *** Test Case 12 ***
#endif // defined(__APPLE__)

/*! @brief Check the response from the 'list' request for this test.
 @param response The response to be analyzed.
 @returns @c true if the expected values are all present and @c false if they are not or if
 unexpected values appear. */
static bool checkResponseFromEchoFromServiceWithRequestHandlerAndInfo(const ServiceResponse &
                                                                                        response)
{
    OD_LOG_ENTER(); //####
    bool result = false;
    
    try
    {
        if (3 <= response.count())
        {
            bool sawChannels = false;
            bool sawClients = false;
            bool sawCount = false;
            bool sawDetach = false;
            bool sawEcho = false;
            bool sawInfo = false;
            bool sawList = false;
            bool sawName = false;
            
            result = true;
            for (int ii = 0; result && (ii < response.count()); ++ii)
            {
                yarp::os::Value anElement(response.element(ii));
                
                if (anElement.isDict())
                {
                    yarp::os::Property * asDict = anElement.asDict();
                    bool                 hasInput = asDict->check(MpM_REQREP_DICT_INPUT_KEY);
                    bool                 hasOutput = asDict->check(MpM_REQREP_DICT_OUTPUT_KEY);
                    
                    if (asDict->check(MpM_REQREP_DICT_REQUEST_KEY))
                    {
                        yarp::os::ConstString aName =
                                            asDict->find(MpM_REQREP_DICT_REQUEST_KEY).asString();
                        
                        if (aName == MpM_CHANNELS_REQUEST)
                        {
                            if (sawChannels)
                            {
                                result = false;
                            }
                            else if ((! hasInput) && hasOutput)
                            {
                                yarp::os::ConstString itsOutput =
                                                asDict->find(MpM_REQREP_DICT_OUTPUT_KEY).asString();
                                
                                sawChannels = (itsOutput == "(s*)(s*)");
                            }
                        }
                        else if (aName == MpM_CLIENTS_REQUEST)
                        {
                            if (sawClients)
                            {
                                result = false;
                            }
                            else if ((! hasInput) && hasOutput)
                            {
                                yarp::os::ConstString itsOutput =
                                                asDict->find(MpM_REQREP_DICT_OUTPUT_KEY).asString();
                                
                                sawClients = (itsOutput == "(s*)");
                            }
                        }
                        else if (aName == MpM_COUNT_REQUEST)
                        {
                            if (sawCount)
                            {
                                result = false;
                            }
                            else if ((! hasInput) && hasOutput)
                            {
                                yarp::os::ConstString itsOutput =
                                                asDict->find(MpM_REQREP_DICT_OUTPUT_KEY).asString();
                                
                                sawCount = (itsOutput == "id");
                            }
                        }
                        else if (aName == MpM_DETACH_REQUEST)
                        {
                            if (sawDetach)
                            {
                                result = false;
                            }
                            else if ((! hasInput) && (! hasOutput))
                            {
                                sawDetach = true;
                            }
                        }
                        else if (aName == MpM_ECHO_REQUEST)
                        {
                            if (sawEcho)
                            {
                                result = false;
                            }
                            else if (hasInput && hasOutput)
                            {
                                yarp::os::ConstString itsOutput =
                                                asDict->find(MpM_REQREP_DICT_OUTPUT_KEY).asString();
                                yarp::os::ConstString itsInput =
                                                asDict->find(MpM_REQREP_DICT_INPUT_KEY).asString();
                                
                                sawEcho = ((itsInput == ".*") && (itsOutput == ".*"));
                            }
                        }
                        else if (aName == MpM_INFO_REQUEST)
                        {
                            if (sawInfo)
                            {
                                result = false;
                            }
                            else if (hasInput && hasOutput)
                            {
                                yarp::os::ConstString itsOutput =
                                                asDict->find(MpM_REQREP_DICT_OUTPUT_KEY).asString();
                                yarp::os::ConstString itsInput =
                                                asDict->find(MpM_REQREP_DICT_INPUT_KEY).asString();
                                
                                sawInfo = ((itsInput == ".") && (itsOutput == "([]?)"));
                            }
                        }
                        else if (aName == MpM_LIST_REQUEST)
                        {
                            if (sawList)
                            {
                                result = false;
                            }
                            else if ((! hasInput) && hasOutput)
                            {
                                yarp::os::ConstString itsOutput =
                                                asDict->find(MpM_REQREP_DICT_OUTPUT_KEY).asString();
                                
                                sawList = (itsOutput == "([]+)");
                            }
                        }
                        else if (aName == MpM_NAME_REQUEST)
                        {
                            if (sawName)
                            {
                                result = false;
                            }
                            else if ((! hasInput) && hasOutput)
                            {
                                yarp::os::ConstString itsOutput =
                                                asDict->find(MpM_REQREP_DICT_OUTPUT_KEY).asString();
                                
                                sawName = (itsOutput == "ssssss");
                            }
                        }
                    }
                    else
                    {
                        result = false;
                    }
                }
                else
                {
                    result = false;
                }
            }
            result &= (sawChannels && sawClients && sawCount && sawDetach && sawEcho && sawInfo &&
                       sawList && sawName);
        }
        else
        {
            // Wrong number of values in the response.
            OD_LOG("! (3 <= response.count())"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // checkResponseFromEchoFromServiceWithRequestHandlerAndInfo

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestRequestEchoFromServiceWithRequestHandlerAndInfo(const char * launchPath,
                                                                 const int    argc,
                                                                 char * *     argv) // send 'list'
                                                                                    // request
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        Test12Service * stuff = new Test12Service(launchPath, argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                ClientChannel * outChannel = doCreateTestChannel(stuff->getEndpoint(),
                                                                 "test/requestechofromservice"
                                                                 "withrequesthandlerandinfo_");
                
                if (outChannel)
                {
                    ServiceRequest  request(MpM_LIST_REQUEST);
                    ServiceResponse response;
                    
                    if (request.send(*outChannel, &response))
                    {
                        OD_LOG_LL1("response size = ", response.count()); //####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_LOG_S1s("response value = ", response.element(ii).toString()); //####
                        }
                        if (checkResponseFromEchoFromServiceWithRequestHandlerAndInfo(response))
                        {
                            result = 0;
                        }
                        else
                        {
                            OD_LOG("! (checkResponseFromEchoFromServiceWithRequestHandler" //####
                                   "AndInfo(response))"); //####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request.send(*outChannel, &response))"); //####
                    }
                    doDestroyTestChannel(stuff->getEndpoint(), outChannel);
                    outChannel = NULL;
                }
                else
                {
                    OD_LOG("! (outChannel)"); //####
                }
                stuff->stop();
            }
            else
            {
                OD_LOG("! (stuff->start())"); //####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_L(result); //####
    return result;
} // doTestRequestEchoFromServiceWithRequestHandlerAndInfo

/*! @brief The signal handler to catch requests to stop the service.
 @param signal The signal being handled. */
static void catchSignal(int signal)
{
    OD_LOG_ENTER(); //####
    OD_LOG_LL1("signal = ", signal); //####
#if MAC_OR_LINUX_
    char numBuff[30];
#endif // MAC_OR_LINUX_
    
#if MAC_OR_LINUX_
    snprintf(numBuff, sizeof(numBuff), "%d", signal);
    GetLogger().error(yarp::os::ConstString("Exiting due to signal ") + numBuff +
                                      yarp::os::ConstString(" = ") + NameOfSignal(signal));
#else // ! MAC_OR_LINUX_
//    sprintf_s(numBuff, sizeof(numBuff), "%d", signal);
#endif // ! MAC_OR_LINUX_
    OD_LOG_EXIT_EXIT(1); //####
    yarp::os::exit(1);
} // catchSignal

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for unit tests of the M+M common classes.
 
 The first argument is the test number, the second argument is the name of the channel to be used
 with the test, the optional third argument is the machine to connect to and the optional fourth
 argument is the port number to be used. Output depends on the test being run.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the unit tests.
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
    int result = 1;
    
    try
    {
        Utilities::SetUpGlobalStatusReporter();
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork())
#endif // CheckNetworkWorks_
        {
            yarp::os::Network yarp; // This is necessary to establish any connections to the YARP
                                    // infrastructure
            
            Initialize(*argv);
            if (0 < --argc)
            {
                int selector = atoi(argv[1]);
                
                SetSignalHandlers(catchSignal);
                switch (selector)
                {
                    case 0 :
                        // Just used to validate the random number seed.
                        result = 0;
                        break;
                        
                    case 1 :
                        result = doTestCreateEndpoint(*argv, argc - 1, argv + 2);
                        break;
                        
                    case 2 :
                        result = doTestConnectToEndpoint(*argv, argc - 1, argv + 2);
                        break;
                        
                    case 3 :
                        result = doTestWriteToEndpoint(*argv, argc - 1, argv + 2);
                        break;
                        
                    case 4 :
                        result = doTestEchoFromEndpointWithReader(*argv, argc - 1, argv + 2);
                        break;
                        
                    case 5 :
                        result = doTestEchoFromEndpointWithReaderCreator(*argv, argc - 1, argv + 2);
                        break;
                        
                    case 6 :
                        result = doTestCreateRequest(*argv, argc - 1, argv + 2);
                        break;
                        
                    case 7 :
                        result = doTestCreateResponse(*argv, argc - 1, argv + 2);
                        break;
                        
                    case 8 :
                        result = doTestRequestEchoFromEndpoint(*argv, argc - 1, argv + 2);
                        break;
                        
                    case 9 :
                        result = doTestRequestEchoFromServiceUsingDefaultWithReader(*argv, argc - 1,
                                                                                    argv + 2);
                        break;
                        
                    case 10 :
                        result = doTestRequestEchoFromServiceUsingDefaultWithReaderCreator(*argv,
                                                                                           argc - 1,
                                                                                       argv + 2);
                        break;
                        
                    case 11 :
                        result = doTestRequestEchoFromServiceWithRequestHandler(*argv, argc - 1,
                                                                                argv + 2);
                        break;
                        
                    case 12 :
                        result = doTestRequestEchoFromServiceWithRequestHandlerAndInfo(*argv,
                                                                                       argc - 1,
                                                                                       argv + 2);
                        break;
                        
                    default :
                        break;
                        
                }
                if (result)
                {
                    OD_LOG_LL1("%%%%%%% unit test failure = ", result); //####
                }
            }
            else
            {
                OD_LOG("! (0 < --argc)"); //####
            }
        }
#if CheckNetworkWorks_
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork())"); //####
# if MAC_OR_LINUX_
            GetLogger().fail("YARP network not running.");
# else // ! MAC_OR_LINUX_
            std::cerr << "YARP network not running." << std::endl;
# endif // ! MAC_OR_LINUX_
        }
#endif // CheckNetworkWorks_
        Utilities::ShutDownGlobalStatusReporter();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(result); //####
    return result;
} // main
