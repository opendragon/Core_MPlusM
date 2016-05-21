//--------------------------------------------------------------------------------------------------
//
//  File:       CommonTests/m+mCommonTest.cpp
//
//  Project:    m+m
//
//  Contains:   The test driver for the unit tests of the m+m common library.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#include "m+mTest03Handler.hpp"
#include "m+mTest04Handler.hpp"
#include "m+mTest05HandlerCreator.hpp"
#include "m+mTest08Handler.hpp"
#include "m+mTest09Service.hpp"
#include "m+mTest10Service.hpp"
#include "m+mTest11Service.hpp"
#include "m+mTest12Service.hpp"

#include <m+m/m+mClientChannel.hpp>
#include <m+m/m+mEndpoint.hpp>
#include <m+m/m+mRequests.hpp>
#include <m+m/m+mServiceRequest.hpp>
#include <m+m/m+mServiceResponse.hpp>
#include <m+m/m+mUtilities.hpp>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The test driver for the unit tests of the m+m common library. */

/*! @namespace MplusM::Test
 @brief The classes used for unit testing of the m+m classes. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Test;
using std::cerr;
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

/*! @brief Create an endpoint for a test.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the endpoint constructor.
 @returns A newly created endpoint, or @c NULL if one could not be created. */
static Endpoint *
doCreateEndpointForTest(const int argc,
                        char * *  argv)
{
    ODL_ENTER(); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
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
        else
        {
            ODL_LOG("! (0 < argc)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_P(stuff); //####
    return stuff;
} // doCreateEndpointForTest

/*! @brief Create a temporary channel for a test.
 @param[in] destinationName The name of the channel to be connected to.
 @param[in] channelPath The root path for the new temporary channel.
 @returns A pointer to a newly-allocated temporary channel. */
static ClientChannel *
doCreateTestChannel(const YarpString & destinationName,
                    const char *       channelPath)
{
    ODL_ENTER(); //####
    ODL_S2("destinationName = ", destinationName.c_str(), "channelPath = ", channelPath); //####
    YarpString              aName(GetRandomChannelName(channelPath));
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
        if (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME_))
        {
            if (! Utilities::NetworkConnectWithRetries(aName, destinationName, STANDARD_WAIT_TIME_))
            {
                ODL_LOG("(! Utilities::NetworkConnectWithRetries(aName, destinationName, " //####
                        "STANDARD_WAIT_TIME_))"); //####
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
                BaseChannel::RelinquishChannel(newChannel);
                newChannel = NULL;
            }
        }
        else
        {
            ODL_LOG("! (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME_))"); //####
        }
    }
    else
    {
        ODL_LOG("! (newChannel)"); //####
    }
    ODL_EXIT_P(newChannel); //####
    return newChannel;
} // doCreateTestChannel

/*! @brief Create a temporary channel for a test.
 @param[in] anEndpoint The endpoint to be connected to.
 @param[in] channelPath The root path for the new temporary channel.
 @returns A pointer to a newly-allocated temporary channel. */
static ClientChannel *
doCreateTestChannel(Endpoint &   anEndpoint,
                    const char * channelPath)
{
    return doCreateTestChannel(anEndpoint.getName(), channelPath);
} // doCreateTestChannel

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief Destroy a temporary channel that was used with a test.
 @param[in] destinationName The name of the channel that the temporary channel was connected to.
 @param[in] theChannel A pointer to the temporary channel. */
static void
doDestroyTestChannel(const YarpString & destinationName,
                     ClientChannel *    theChannel)
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(destinationName)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    ODL_ENTER(); //####
    ODL_P1("theChannel = ", theChannel); //####
    if (theChannel)
    {
#if defined(MpM_DoExplicitDisconnect)
        if (! Utilities::NetworkDisconnectWithRetries(theChannel->name(), destinationName,
                                                      STANDARD_WAIT_TIME_))
        {
            ODL_LOG("(! Utilities::NetworkDisconnectWithRetries(theChannel->name(), " //####
                    "destinationName, STANDARD_WAIT_TIME_))"); //####
        }
#endif // defined(MpM_DoExplicitDisconnect)
#if defined(MpM_DoExplicitClose)
        theChannel->close();
#endif // defined(MpM_DoExplicitClose)
        BaseChannel::RelinquishChannel(theChannel);
    }
    ODL_EXIT(); //####
} // doDestroyTestChannel
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

/*! @brief Destroy a temporary channel that was used with a test.
 @param[in] anEndpoint The endpoint to be connected to.
 @param[in] theChannel A pointer to the temporary channel. */
static void
doDestroyTestChannel(Endpoint &      anEndpoint,
                     ClientChannel * theChannel)
{
    doDestroyTestChannel(anEndpoint.getName(), theChannel);
} // doDestroyTestChannel

#if defined(__APPLE__)
# pragma mark *** Test Case 01 ***
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestCreateEndpoint(const char * launchPath,
                     const int    argc,
                     char * *     argv) // create endpoint
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    int result = 1;

    try
    {
        Endpoint * stuff = doCreateEndpointForTest(argc, argv);

        if (stuff)
        {
            if (stuff->open(STANDARD_WAIT_TIME_))
            {
                ODL_S1s("endpoint name = ", stuff->getName());
                result = 0;
            }
            else
            {
                ODL_LOG("! (stuff->open(STANDARD_WAIT_TIME_))"); //####
            }
            delete stuff;
        }
        else
        {
            ODL_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestCreateEndpoint
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark *** Test Case 02 ***
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestConnectToEndpoint(const char * launchPath,
                        const int    argc,
                        char * *     argv) // connect to endpoint
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    int result = 1;

    try
    {
        Endpoint *              stuff = doCreateEndpointForTest(argc, argv);
        ChannelStatusReporter & reporter = *Utilities::GetGlobalStatusReporter();

        if (stuff)
        {
            if (stuff->open(STANDARD_WAIT_TIME_) && stuff->setReporter(reporter, true))
            {
                ODL_S1s("endpoint name = ", stuff->getName());
                // Now we try to connect!
                YarpString      aName(GetRandomChannelName("_test_/connecttoendpoint_"));
                ClientChannel * outChannel = new ClientChannel;

                if (outChannel)
                {
#if defined(MpM_ReportOnConnections)
                    outChannel->setReporter(reporter);
                    outChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
                    if (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME_))
                    {
                        outChannel->getReport(reporter);
                        ODL_S1s("endpoint name = ", stuff->getName());
                        if (outChannel->addOutputWithRetries(stuff->getName(), STANDARD_WAIT_TIME_))
                        {
                            result = 0;
#if defined(MpM_DoExplicitDisconnect)
                            if (! NetworkDisconnectWithRetries(outChannel->name(), stuff->getName(),
                                                               STANDARD_WAIT_TIME_))
                            {
                                ODL_LOG("(! NetworkDisconnectWithRetries(" //####
                                        "outChannel->name(), stuff->getName(), "
                                        "STANDARD_WAIT_TIME_))"); //####
                            }
#endif // defined(MpM_DoExplicitDisconnect)
                        }
                        else
                        {
                            ODL_LOG("! (outChannel->addOutputWithRetries(stuff->getName(), " //####
                                    "STANDARD_WAIT_TIME_))"); //####
                        }
#if defined(MpM_DoExplicitClose)
                        outChannel->close();
#endif // defined(MpM_DoExplicitClose)
                    }
                    else
                    {
                        ODL_LOG("! (outChannel->openWithRetries(aName, " //####
                                "STANDARD_WAIT_TIME_))"); //####
                    }
                    BaseChannel::RelinquishChannel(outChannel);
                }
                else
                {
                    ODL_LOG("! (outChannel)");
                }
            }
            else
            {
                ODL_LOG("! (stuff->open(STANDARD_WAIT_TIME_) && " //####
                        "stuff->setReporter(reporter, true))"); //####
            }
            delete stuff;
        }
        else
        {
            ODL_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestConnectToEndpoint
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark *** Test Case 03 ***
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestWriteToEndpoint(const char * launchPath,
                      const int    argc,
                      char * *     argv) // send to endpoint
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    int result = 1;

    try
    {
        Endpoint *              stuff = doCreateEndpointForTest(argc, argv);
        ChannelStatusReporter & reporter = *Utilities::GetGlobalStatusReporter();

        if (stuff)
        {
            Test03Handler handler;

            if (stuff->setInputHandler(handler) && stuff->open(STANDARD_WAIT_TIME_) &&
                stuff->setReporter(reporter, true))
            {
                ODL_S1s("endpoint name = ", stuff->getName());
                // Now we try to connect!
                YarpString      aName(GetRandomChannelName("_test_/writetoendpoint_"));
                ClientChannel * outChannel = new ClientChannel;

                if (outChannel)
                {
#if defined(MpM_ReportOnConnections)
                    outChannel->setReporter(reporter);
                    outChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
                    if (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME_))
                    {
                        outChannel->getReport(reporter);
                        if (outChannel->addOutputWithRetries(stuff->getName(), STANDARD_WAIT_TIME_))
                        {
                            yarp::os::Bottle message;

                            message.addString(aName);
                            message.addString("howdi");
                            if (outChannel->writeBottle(message))
                            {
                                result = 0;
#if defined(MpM_DoExplicitDisconnect)
                                if (! NetworkDisconnectWithRetries(outChannel->name(),
                                                                   stuff->getName(),
                                                                   STANDARD_WAIT_TIME_))
                                {
                                    ODL_LOG("(! NetworkDisconnectWithRetries(outChannel->" //####
                                            "name(), stuff->getName(), " //####
                                            "STANDARD_WAIT_TIME_))"); //####
                                }
#endif // defined(MpM_DoExplicitDisconnect)
                            }
                            else
                            {
                                ODL_LOG("! (outChannel->writeBottle(message))"); //####
#if defined(MpM_StallOnSendProblem)
                                Stall();
#endif // defined(MpM_StallOnSendProblem)
                            }
                        }
                        else
                        {
                            ODL_LOG("! (outChannel->addOutputWithRetries(stuff->getName(), " //####
                                    "STANDARD_WAIT_TIME_))"); //####
                        }
#if defined(MpM_DoExplicitClose)
                        outChannel->close();
#endif // defined(MpM_DoExplicitClose)
                    }
                    else
                    {
                        ODL_LOG("! (outChannel->openWithRetries(aName, " //####
                                "STANDARD_WAIT_TIME_))"); //####
                    }
                    BaseChannel::RelinquishChannel(outChannel);
                }
                else
                {
                    ODL_LOG("! (outChannel)");
                }
            }
            else
            {
                ODL_LOG("! (stuff->setInputHandler(handler) && " //####
                        "stuff->open(STANDARD_WAIT_TIME_) && " //####
                        "stuff->setReporter(reporter, true))"); //####
            }
            delete stuff;
        }
        else
        {
            ODL_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestWriteToEndpoint
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark *** Test Case 04 ***
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestEchoFromEndpointWithReader(const char * launchPath,
                                 const int    argc,
                                 char * *     argv) // send to endpoint
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    int result = 1;

    try
    {
        Endpoint *              stuff = doCreateEndpointForTest(argc, argv);
        ChannelStatusReporter & reporter = *Utilities::GetGlobalStatusReporter();

        if (stuff)
        {
            Test04Handler handler;

            if (stuff->setInputHandler(handler) && stuff->open(STANDARD_WAIT_TIME_) &&
                stuff->setReporter(reporter, true))
            {
                ODL_S1s("endpoint name = ", stuff->getName());
                // Now we try to connect!
                YarpString      aName(GetRandomChannelName("_test_/echofromendpointwithreader_"));
                ClientChannel * outChannel = new ClientChannel;

                if (outChannel)
                {
#if defined(MpM_ReportOnConnections)
                    outChannel->setReporter(reporter);
                    outChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
                    if (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME_))
                    {
                        outChannel->getReport(reporter);
                        if (outChannel->addOutputWithRetries(stuff->getName(), STANDARD_WAIT_TIME_))
                        {
                            yarp::os::Bottle message;
                            yarp::os::Bottle response;

                            message.addString(aName);
                            message.addString("howdi");
                            if (outChannel->writeBottle(message, response))
                            {
                                result = 0;
#if defined(MpM_DoExplicitDisconnect)
                                if (! NetworkDisconnectWithRetries(outChannel->name(),
                                                                   stuff->getName(),
                                                                   STANDARD_WAIT_TIME_))
                                {
                                    ODL_LOG("(! NetworkDisconnectWithRetries(outChannel->" //####
                                            "name(), stuff->getName(), " //####
                                            "STANDARD_WAIT_TIME_))"); //####
                                }
#endif // defined(MpM_DoExplicitDisconnect)
                            }
                            else
                            {
                                ODL_LOG("! (outChannel->writeBottle(message, response))"); //####
#if defined(MpM_StallOnSendProblem)
                                Stall();
#endif // defined(MpM_StallOnSendProblem)
                            }
                        }
                        else
                        {
                            ODL_LOG("! (outChannel->addOutputWithRetries(stuff->getName(), " //####
                                    "STANDARD_WAIT_TIME_))"); //####
                        }
#if defined(MpM_DoExplicitClose)
                        outChannel->close();
#endif // defined(MpM_DoExplicitClose)
                    }
                    else
                    {
                        ODL_LOG("! (outChannel->openWithRetries(aName, " //####
                                "STANDARD_WAIT_TIME_))"); //####
                    }
                    BaseChannel::RelinquishChannel(outChannel);
                }
                else
                {
                    ODL_LOG("! (outChannel)");
                }
            }
            else
            {
                ODL_LOG("! (stuff->setInputHandler(handler) && " //####
                        "stuff->open(STANDARD_WAIT_TIME_) && " //####
                        "stuff->setReporter(reporter, true))"); //####
            }
            delete stuff;
        }
        else
        {
            ODL_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestEchoFromEndpointWithReader
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark *** Test Case 05 ***
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestEchoFromEndpointWithReaderCreator(const char * launchPath,
                                        const int    argc,
                                        char * *     argv) // send to endpoint
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    int result = 1;

    try
    {
        Endpoint *              stuff = doCreateEndpointForTest(argc, argv);
        ChannelStatusReporter & reporter = *Utilities::GetGlobalStatusReporter();

        if (stuff)
        {
            Test05HandlerCreator handlerCreator;

            if (stuff->setInputHandlerCreator(handlerCreator) && stuff->open(STANDARD_WAIT_TIME_) &&
                stuff->setReporter(reporter, true))
            {
                ODL_S1s("endpoint name = ", stuff->getName());
                // Now we try to connect!
                YarpString      aName(GetRandomChannelName("_test_/echofromendpointwithreader"
                                                           "creator_"));
                ClientChannel * outChannel = new ClientChannel;

                if (outChannel)
                {
#if defined(MpM_ReportOnConnections)
                    outChannel->setReporter(reporter);
                    outChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
                    if (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME_))
                    {
                        outChannel->getReport(reporter);
                        if (outChannel->addOutputWithRetries(stuff->getName(), STANDARD_WAIT_TIME_))
                        {
                            yarp::os::Bottle message;
                            yarp::os::Bottle response;

                            message.addString(aName);
                            message.addString("howdi");
                            if (outChannel->writeBottle(message, response))
                            {
                                result = 0;
#if defined(MpM_DoExplicitDisconnect)
                                if (! NetworkDisconnectWithRetries(outChannel->name(),
                                                                   stuff->getName(),
                                                                   STANDARD_WAIT_TIME_))
                                {
                                    ODL_LOG("(! NetworkDisconnectWithRetries(outChannel->" //####
                                            "name(), stuff->getName(), " //####
                                            "STANDARD_WAIT_TIME_))"); //####
                                }
#endif // defined(MpM_DoExplicitDisconnect)
                            }
                            else
                            {
                                ODL_LOG("! (outChannel->writeBottle(message, response))"); //####
#if defined(MpM_StallOnSendProblem)
                                Stall();
#endif // defined(MpM_StallOnSendProblem)
                            }
                        }
                        else
                        {
                            ODL_LOG("! (outChannel->addOutputWithRetries(stuff->getName(), " //####
                                    "STANDARD_WAIT_TIME_))"); //####
                        }
#if defined(MpM_DoExplicitClose)
                        outChannel->close();
#endif // defined(MpM_DoExplicitClose)
                    }
                    else
                    {
                        ODL_LOG("! (outChannel->openWithRetries(aName, " //####
                                "STANDARD_WAIT_TIME_))"); //####
                    }
                    BaseChannel::RelinquishChannel(outChannel);
                }
                else
                {
                    ODL_LOG("! (outChannel)");
                }
            }
            else
            {
                ODL_LOG("! (stuff->setInputHandlerCreator(handlerCreator) && " //####
                        "stuff->open(STANDARD_WAIT_TIME_) && " //####
                        "stuff->setReporter(&reporter, true))"); //####
            }
            delete stuff;
        }
        else
        {
            ODL_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestEchoFromEndpointWithReaderCreator
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark *** Test Case 06 ***
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestCreateRequest(const char * launchPath,
                    const int    argc,
                    char * *     argv) // create request
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    int result = 1;

    try
    {
        if (0 == argc)
        {
            ODL_LOG("0 == argc"); //####
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
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestCreateRequest
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark *** Test Case 07 ***
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestCreateResponse(const char * launchPath,
                     const int    argc,
                     char * *     argv) // create request
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
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
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestCreateResponse
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark *** Test Case 08 ***
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestRequestEchoFromEndpoint(const char * launchPath,
                              const int    argc,
                              char * *     argv) // create request
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(launchPath)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    int result = 1;

    try
    {
        Endpoint *              stuff = doCreateEndpointForTest(argc, argv);
        ChannelStatusReporter & reporter = *Utilities::GetGlobalStatusReporter();

        if (stuff)
        {
            Test08Handler handler;

            if (stuff->setInputHandler(handler) && stuff->open(STANDARD_WAIT_TIME_) &&
                stuff->setReporter(reporter, true))
            {
                ClientChannel * outChannel = doCreateTestChannel(stuff->getName(),
                                                                 "test/requestechofromendpoint_");

                if (outChannel)
                {
                    ODL_S1s("endpoint name = ", stuff->getName());
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(MpM_ECHO_REQUEST_, parameters);
                    ServiceResponse  response;

                    if (request.send(*outChannel, response))
                    {
                        ODL_LL1("response size = ", response.count()); //####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            ODL_S1s("response value = ", response.element(ii).toString()); //####
                        }
                        result = 0;
                    }
                    else
                    {
                        ODL_LOG("! (request.send(*outChannel, response))"); //####
                    }
                    doDestroyTestChannel(stuff->getName(), outChannel);
                    outChannel = NULL;
                }
                else
                {
                    ODL_LOG("! (outChannel)"); //####
                }
            }
            else
            {
                ODL_LOG("! (stuff->setInputHandler(handler) && " //####
                        "stuff->open(STANDARD_WAIT_TIME_) && " //####
                        "stuff->setReporter(reporter, true))"); //####
            }
            delete stuff;
        }
        else
        {
            ODL_LOG("! (stuff)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestRequestEchoFromEndpoint
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark *** Test Case 09 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestRequestEchoFromServiceUsingDefaultWithReader(const char * launchPath,
                                                   const int    argc,
                                                   char * *     argv) // send 'echo' request
{
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    int result = 1;

    try
    {
        Test09Service * aService = new Test09Service(launchPath, argc, argv);

        if (aService)
        {
            if (aService->startService())
            {
                ClientChannel * outChannel = doCreateTestChannel(aService->getEndpoint(),
                                                                 "test/requestechofromservice"
                                                                 "usingdefaultwithreader");

                if (outChannel)
                {
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(MpM_ECHO_REQUEST_, parameters);
                    ServiceResponse  response;

                    if (request.send(*outChannel, response))
                    {
                        ODL_LL1("response size = ", response.count()); //####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            ODL_S1s("response value = ", response.element(ii).toString()); //####
                        }
                        result = 0;
                    }
                    else
                    {
                        ODL_LOG("! (request.send(*outChannel, response))"); //####
                    }
                    doDestroyTestChannel(aService->getEndpoint(), outChannel);
                    outChannel = NULL;
                }
                else
                {
                    ODL_LOG("! (outChannel)"); //####
                }
                aService->stopService();
            }
            else
            {
                ODL_LOG("! (aService->startService())"); //####
            }
            delete aService;
        }
        else
        {
            ODL_LOG("! (aService)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestRequestEchoFromServiceUsingDefaultWithReader

#if defined(__APPLE__)
# pragma mark *** Test Case 10 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestRequestEchoFromServiceUsingDefaultWithReaderCreator(const char * launchPath,
                                                          const int    argc,
                                                          char * *     argv) // send 'echo'
                                                                             // request
{
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    int result = 1;

    try
    {
        Test10Service * aService = new Test10Service(launchPath, argc, argv);

        if (aService)
        {
            if (aService->startService())
            {
                ClientChannel * outChannel = doCreateTestChannel(aService->getEndpoint(),
                                                                 "test/requestechofromservice"
                                                                 "usingdefaultwithreadercreator_");

                if (outChannel)
                {
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(MpM_ECHO_REQUEST_, parameters);
                    ServiceResponse  response;

                    if (request.send(*outChannel, response))
                    {
                        ODL_LL1("response size = ", response.count()); //####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            ODL_S1s("response value = ", response.element(ii).toString()); //####
                        }
                        result = 0;
                    }
                    else
                    {
                        ODL_LOG("! (request.send(*outChannel, response))"); //####
                    }
                    doDestroyTestChannel(aService->getEndpoint(), outChannel);
                    outChannel = NULL;
                }
                else
                {
                    ODL_LOG("! (outChannel)"); //####
                }
                aService->stopService();
            }
            else
            {
                ODL_LOG("! (aService->startService())"); //####
            }
            delete aService;
        }
        else
        {
            ODL_LOG("! (aService)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestRequestEchoFromServiceUsingDefaultWithReaderCreator

#if defined(__APPLE__)
# pragma mark *** Test Case 11 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestRequestEchoFromServiceWithRequestHandler(const char * launchPath,
                                               const int    argc,
                                               char * *     argv) // create 'echo' request
{
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    int result = 1;

    try
    {
        Test11Service * aService = new Test11Service(launchPath, argc, argv);

        if (aService)
        {
            if (aService->startService())
            {
                ClientChannel * outChannel = doCreateTestChannel(aService->getEndpoint(),
                                                                 "test/requestechofromservice"
                                                                 "withrequesthandler_");

                if (outChannel)
                {
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(MpM_ECHO_REQUEST_, parameters);
                    ServiceResponse  response;

                    if (request.send(*outChannel, response))
                    {
                        if (3 == response.count())
                        {
                            YarpString expected[] =
                            {
                                "some", "to", "send"
                            };

                            result = 0;
                            for (int ii = 0; (! result) && (ii < response.count()); ++ii)
                            {
                                if (expected[ii] != response.element(ii).toString())
                                {
                                    ODL_S2s("expected[ii] = ", expected[ii], //####
                                            "response.element(ii).toString() = ", //####
                                            response.element(ii).toString()); //####
                                    result = 1;
                                }
                            }
                        }
                        else
                        {
                            ODL_LOG("! (3 == response.count())"); //####
                        }
                    }
                    else
                    {
                        ODL_LOG("! (request.send(*outChannel, response))"); //####
                    }
                    doDestroyTestChannel(aService->getEndpoint(), outChannel);
                    outChannel = NULL;
                }
                else
                {
                    ODL_LOG("! (outChannel)"); //####
                }
                aService->stopService();
            }
            else
            {
                ODL_LOG("! (aService->startService())"); //####
            }
            delete aService;
        }
        else
        {
            ODL_LOG("! (aService)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestRequestEchoFromServiceWithRequestHandler

#if defined(__APPLE__)
# pragma mark *** Test Case 12 ***
#endif // defined(__APPLE__)

/*! @brief Check the response from the 'list' request for this test.
 @param[in,out] asDict The dictionary to be checked.
 @param[out] sawArguments Set to @c true if a valid 'arguments' entry appears.
 @param[out] sawChannels Set to @c true if a valid 'channels' entry appears.
 @param[out] sawClients Set to @c true if a valid 'clients' entry appears.
 @param[out] sawDetach Set to @c true if a valid 'detach' entry appears.
 @param[out] sawEcho Set to @c true if a valid 'echo' entry appears.
 @param[out] sawExtraInfo Set to @c true if a valid 'extraInfo' entry appears.
 @param[out] sawInfo Set to @c true if a valid 'info' entry appears.
 @param[out] sawList Set to @c true if a valid 'list' entry appears.
 @param[out] sawMetrics Set to @c true if a valid 'getMetrics' entry appears.
 @param[out] sawMetricsState Set to @c true if a valid 'getMetricsState' entry appears.
 @param[out] sawName Set to @c true if a valid 'name' entry appears.
 @param[out] sawSetMetricsState Set to @c true if a valid 'setMetricsState' entry appears.
 @returns @c false if an unexpected value appears and @c true otherwise. */
static bool
checkListDictionary(yarp::os::Property & asDict,
                    bool &               sawArguments,
                    bool &               sawChannels,
                    bool &               sawClients,
                    bool &               sawDetach,
                    bool &               sawEcho,
                    bool &               sawExtraInfo,
                    bool &               sawInfo,
                    bool &               sawList,
                    bool &               sawMetrics,
                    bool &               sawMetricsState,
                    bool &               sawName,
                    bool &               sawSetMetricsState)
{
    ODL_ENTER(); //####
    ODL_P4("asDict = ", &asDict, "sawArguments = ", &sawArguments, "sawChannels = ", //####
           &sawChannels, "sawClients = ", &sawClients); //####
    ODL_P4("sawDetach = ", &sawDetach, "sawEcho = ", &sawEcho, "sawExtraInfo = ", //####
           &sawExtraInfo, "sawInfo = ", &sawInfo); //####
    ODL_P4("sawList = ", &sawList, "sawMetrics = ", &sawMetrics, "sawMetricsState = ", //####
           &sawMetricsState, "sawName = ", &sawName); //####
    ODL_P1("sawSetMetricsState = ", &sawSetMetricsState); //####
    bool result = true;
    bool hasInput = asDict.check(MpM_REQREP_DICT_INPUT_KEY_);
    bool hasOutput = asDict.check(MpM_REQREP_DICT_OUTPUT_KEY_);

    if (asDict.check(MpM_REQREP_DICT_REQUEST_KEY_))
    {
        YarpString aName(asDict.find(MpM_REQREP_DICT_REQUEST_KEY_).asString());

        if (aName == MpM_ARGUMENTS_REQUEST_)
        {
            if (sawArguments)
            {
                result = false;
            }
            else if ((! hasInput) && hasOutput)
            {
                YarpString itsOutput(asDict.find(MpM_REQREP_DICT_OUTPUT_KEY_).asString());

                sawArguments = (itsOutput == "s+");
            }
        }
        else if (aName == MpM_CHANNELS_REQUEST_)
        {
            if (sawChannels)
            {
                result = false;
            }
            else if ((! hasInput) && hasOutput)
            {
                YarpString itsOutput(asDict.find(MpM_REQREP_DICT_OUTPUT_KEY_).asString());

                sawChannels = (itsOutput == "(s*)(s*)(s*)");
            }
        }
        else if (aName == MpM_CLIENTS_REQUEST_)
        {
            if (sawClients)
            {
                result = false;
            }
            else if ((! hasInput) && hasOutput)
            {
                YarpString itsOutput(asDict.find(MpM_REQREP_DICT_OUTPUT_KEY_).asString());

                sawClients = (itsOutput == "(s*)");
            }
        }
        else if (aName == MpM_DETACH_REQUEST_)
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
        else if (aName == MpM_ECHO_REQUEST_)
        {
            if (sawEcho)
            {
                result = false;
            }
            else if (hasInput && hasOutput)
            {
                YarpString itsOutput(asDict.find(MpM_REQREP_DICT_OUTPUT_KEY_).asString());
                YarpString itsInput(asDict.find(MpM_REQREP_DICT_INPUT_KEY_).asString());

                sawEcho = ((itsInput == ".*") && (itsOutput == ".*"));
            }
        }
        else if (aName == MpM_EXTRAINFO_REQUEST_)
        {
            if (sawExtraInfo)
            {
                result = false;
            }
            else if ((! hasInput) && hasOutput)
            {
                YarpString itsOutput(asDict.find(MpM_REQREP_DICT_OUTPUT_KEY_).asString());

                sawExtraInfo = (itsOutput == "s");
            }
        }
        else if (aName == MpM_INFO_REQUEST_)
        {
            if (sawInfo)
            {
                result = false;
            }
            else if (hasInput && hasOutput)
            {
                YarpString itsOutput(asDict.find(MpM_REQREP_DICT_OUTPUT_KEY_).asString());
                YarpString itsInput(asDict.find(MpM_REQREP_DICT_INPUT_KEY_).asString());

                sawInfo = ((itsInput == ".") && (itsOutput == "([]?)"));
            }
        }
        else if (aName == MpM_LIST_REQUEST_)
        {
            if (sawList)
            {
                result = false;
            }
            else if ((! hasInput) && hasOutput)
            {
                YarpString itsOutput(asDict.find(MpM_REQREP_DICT_OUTPUT_KEY_).asString());

                sawList = (itsOutput == "([]+)");
            }
        }
        else if (aName == MpM_METRICS_REQUEST_)
        {
            if (sawMetrics)
            {
                result = false;
            }
            else if ((! hasInput) && hasOutput)
            {
                YarpString itsOutput(asDict.find(MpM_REQREP_DICT_OUTPUT_KEY_).asString());

                sawMetrics = (itsOutput == "([]+)");
            }
        }
        else if (aName == MpM_METRICSSTATE_REQUEST_)
        {
            if (sawMetricsState)
            {
                result = false;
            }
            else if ((! hasInput) && hasOutput)
            {
                YarpString itsOutput(asDict.find(MpM_REQREP_DICT_OUTPUT_KEY_).asString());

                sawMetricsState = (itsOutput == "i");
            }
        }
        else if (aName == MpM_NAME_REQUEST_)
        {
            if (sawName)
            {
                result = false;
            }
            else if ((! hasInput) && hasOutput)
            {
                YarpString itsOutput(asDict.find(MpM_REQREP_DICT_OUTPUT_KEY_).asString());

                sawName = (itsOutput == "sssssss");
            }
        }
        else if (aName == MpM_SETMETRICSSTATE_REQUEST_)
        {
            if (sawSetMetricsState)
            {
                result = false;
            }
            else if (hasInput && (! hasOutput))
            {
                YarpString itsInput(asDict.find(MpM_REQREP_DICT_INPUT_KEY_).asString());

                sawSetMetricsState = (itsInput == "i");
            }
        }
    }
    else
    {
        result = false;
    }
    ODL_EXIT_B(result); //####
    return result;
} // checkListDictionary

/*! @brief Check the response from the 'list' request for this test.
 @param[in] response The response to be analyzed.
 @returns @c true if the expected values are all present and @c false if they are not or if
 unexpected values appear. */
static bool
checkResponseFromEchoFromServiceWithRequestHandlerAndInfo(const ServiceResponse & response)
{
    ODL_ENTER(); //####
    ODL_P1("response = ", &response); //####
    bool result = false;

    try
    {
        if (3 <= response.count())
        {
            bool sawArguments = false;
            bool sawChannels = false;
            bool sawClients = false;
            bool sawDetach = false;
            bool sawEcho = false;
            bool sawExtraInfo = false;
            bool sawInfo = false;
            bool sawList = false;
            bool sawMetrics = false;
            bool sawMetricsState = false;
            bool sawName = false;
            bool sawSetMetricsState = false;

            result = true;
            for (int ii = 0; result && (ii < response.count()); ++ii)
            {
                yarp::os::Value anElement(response.element(ii));

                if (anElement.isDict())
                {
                    yarp::os::Property * asDict = anElement.asDict();

                    if (asDict)
                    {
                        result = checkListDictionary(*asDict, sawArguments, sawChannels, sawClients,
                                                     sawDetach, sawEcho, sawExtraInfo, sawInfo,
                                                     sawList, sawMetrics, sawMetricsState, sawName,
                                                     sawSetMetricsState);
                    }
                }
                else if (anElement.isList())
                {
                    yarp::os::Bottle * asList = anElement.asList();

                    if (asList)
                    {
                        yarp::os::Property asDict;

                        if (ListIsReallyDictionary(*asList, asDict))
                        {
                            result = checkListDictionary(asDict, sawArguments, sawChannels,
                                                         sawClients, sawDetach, sawEcho,
                                                         sawExtraInfo, sawInfo, sawList, sawMetrics,
                                                         sawMetricsState, sawName,
                                                         sawSetMetricsState);
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
                else
                {
                    result = false;
                }
            }
            result &= (sawArguments && sawChannels && sawClients && sawDetach && sawEcho &&
                       sawExtraInfo && sawInfo && sawList && sawMetrics && sawMetricsState &&
                       sawName && sawSetMetricsState);
        }
        else
        {
            // Wrong number of values in the response.
            ODL_LOG("! (3 <= response.count())"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_B(result); //####
    return result;
} // checkResponseFromEchoFromServiceWithRequestHandlerAndInfo

/*! @brief Perform a test case.
 @param[in] launchPath The command-line name used to launch the service.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestRequestEchoFromServiceWithRequestHandlerAndInfo(const char * launchPath,
                                                      const int    argc,
                                                      char * *     argv) // send 'list' request
{
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    int result = 1;

    try
    {
        Test12Service * aService = new Test12Service(launchPath, argc, argv);

        if (aService)
        {
            if (aService->startService())
            {
                ClientChannel * outChannel = doCreateTestChannel(aService->getEndpoint(),
                                                                 "test/requestechofromservice"
                                                                 "withrequesthandlerandinfo_");

                if (outChannel)
                {
                    ServiceRequest  request(MpM_LIST_REQUEST_);
                    ServiceResponse response;

                    if (request.send(*outChannel, response))
                    {
                        ODL_LL1("response size = ", response.count()); //####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            ODL_S1s("response value = ", response.element(ii).toString()); //####
                        }
                        if (checkResponseFromEchoFromServiceWithRequestHandlerAndInfo(response))
                        {
                            result = 0;
                        }
                        else
                        {
                            ODL_LOG("! (checkResponseFromEchoFromServiceWithRequestHandler" //####
                                    "AndInfo(response))"); //####
                        }
                    }
                    else
                    {
                        ODL_LOG("! (request.send(*outChannel, response))"); //####
                    }
                    doDestroyTestChannel(aService->getEndpoint(), outChannel);
                    outChannel = NULL;
                }
                else
                {
                    ODL_LOG("! (outChannel)"); //####
                }
                aService->stopService();
            }
            else
            {
                ODL_LOG("! (aService->startService())"); //####
            }
            delete aService;
        }
        else
        {
            ODL_LOG("! (aService)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestRequestEchoFromServiceWithRequestHandlerAndInfo

/*! @brief The signal handler to catch requests to stop the service.
 @param[in] signal The signal being handled. */
static void
catchSignal(int signal)
{
    ODL_ENTER(); //####
    ODL_LL1("signal = ", signal); //####
    std::stringstream buff;
    YarpString        message("Exiting due to signal ");

    buff << signal;
    message += buff.str();
    message += " = ";
    message += NameOfSignal(signal);
    MpM_ERROR_(message.c_str());
    ODL_EXIT_EXIT(1); //####
    yarp::os::exit(1);
} // catchSignal

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for unit tests of the m+m common classes.

 The first argument is the test number, the second argument is the name of the channel to be used
 with the test, the optional third argument is the machine to connect to and the optional fourth
 argument is the port number to be used. Output depends on the test being run.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the unit tests.
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
    int result = 1;

    try
    {
        Utilities::SetUpGlobalStatusReporter();
        Utilities::CheckForNameServerReporter();
        if (Utilities::CheckForValidNetwork())
        {
            yarp::os::Network yarp; // This is necessary to establish any connections to the YARP
                                    // infrastructure

            Initialize(progName);
            if (0 < --argc)
            {
                const char * startPtr = argv[1];
                char *       endPtr;
                int          selector = strtol(startPtr, &endPtr, 10);

                if ((startPtr != endPtr) && (! *endPtr) && (0 < selector))
                {
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
                            result = doTestEchoFromEndpointWithReaderCreator(*argv, argc - 1,
                                                                             argv + 2);
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
                            result = doTestRequestEchoFromServiceUsingDefaultWithReader(*argv,
                                                                                        argc - 1,
                                                                                        argv + 2);
                            break;

                        case 10 :
                            result =
                            doTestRequestEchoFromServiceUsingDefaultWithReaderCreator(*argv,
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
                        ODL_LL1("%%%%%%% unit test failure = ", result); //####
                    }
                }
            }
            else
            {
                ODL_LOG("! (0 < --argc)"); //####
            }
        }
        Utilities::ShutDownGlobalStatusReporter();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    yarp::os::Network::fini();
    ODL_EXIT_L(result); //####
    return result;
} // main
