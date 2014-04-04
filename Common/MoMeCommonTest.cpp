//--------------------------------------------------------------------------------------
//
//  File:       MoMeCommonTest.cpp
//
//  Project:    MoAndMe
//
//  Contains:   The test driver for the unit tests of the MoAndMe common library.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------

#include "CommonTests/MoMeTest03Handler.h"
#include "CommonTests/MoMeTest04Handler.h"
#include "CommonTests/MoMeTest05HandlerCreator.h"
#include "CommonTests/MoMeTest08Handler.h"
#include "CommonTests/MoMeTest09Service.h"
#include "CommonTests/MoMeTest10Service.h"
#include "CommonTests/MoMeTest11Service.h"
#include "CommonTests/MoMeTest12Service.h"
#include "MoMeBaseClient.h"
#include "MoMeBaseRequestHandler.h"
#include "MoMeEndpoint.h"
#include "MoMeEndpointStatusReporter.h"
#include "MoMeRequests.h"
#include "MoMeServiceRequest.h"
#include "MoMeServiceResponse.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#include <iostream>
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#include <yarp/os/all.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The test driver for the unit tests of the MoAndMe common library. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MoAndMe;
using namespace MoAndMe::Common;
using namespace MoAndMe::Test;
using std::cout;
using std::cerr;
using std::endl;

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
    OD_LOG_ENTER();//####
    Endpoint * stuff = NULL;
    
    try
    {
        if (0 < argc)
        {
            switch (argc)
            {
                // Argument order for tests = endpoint name [, IP address / name [, port]]
                case 1:
                    stuff = new Endpoint(*argv);
                    break;
                    
                case 2:
                    stuff = new Endpoint(*argv, argv[1]);
                    break;
                    
                case 3:
                    stuff = new Endpoint(*argv, argv[1], argv[2]);
                    break;
                    
                default:
                    break;
                    
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_P(stuff);//####
    return stuff;
} // doCreateEndpointForTest

/*! @brief Create a temporary channel for a test.
 @param destinationName The name of the channel to be connected to.
 @param channelPath The root path for the new temporary channel.
 @returns A pointer to a newly-allocated temporary channel. */
static Channel * doCreateTestChannel(const yarp::os::ConstString & destinationName,
                                     const char *                  channelPath)
{
    OD_LOG_ENTER();//####
    OD_LOG_S2("destinationName = ", destinationName.c_str(), "channelPath = ", channelPath);//####
    yarp::os::ConstString aName(GetRandomChannelName(channelPath));
    Channel *             newChannel = AcquireChannel();
    
    if (newChannel)
    {
        if (OpenChannelWithRetries(*newChannel, aName))
        {
            if (! NetworkConnectWithRetries(aName, destinationName))
            {
                OD_LOG("(! NetworkConnectWithRetries(aName, destinationName))");//####
#if defined(MAM_DO_EXPLICIT_CLOSE)
                CloseChannel(*newChannel);
#endif // defined(MAM_DO_EXPLICIT_CLOSE)
                RelinquishChannel(newChannel);
            }
        }
        else
        {
            OD_LOG("! (OpenChannelWithRetries(*newChannel, aName))");//####
        }
    }
    else
    {
        OD_LOG("! (newChannel)");//####
    }
    OD_LOG_EXIT_P(newChannel);//####
    return newChannel;
} // doCreateTestChannel

/*! @brief Create a temporary channel for a test.
 @param anEndpoint The endpoint to be connected to.
 @param channelPath The root path for the new temporary channel.
 @returns A pointer to a newly-allocated temporary channel. */
static Channel * doCreateTestChannel(Endpoint &   anEndpoint,
                                     const char * channelPath)
{
    return doCreateTestChannel(anEndpoint.getName(), channelPath);
} // doCreateTestChannel

/*! @brief Destroy a temporary channel that was used with a test.
 @param destinationName The name of the channel that the temporary channel was connected to.
 @param theChannel A pointer to the temporary channel. */
static void doDestroyTestChannel(const yarp::os::ConstString & destinationName,
                                 Channel *                     theChannel)
{
#if (! defined(MAM_DO_EXPLICIT_DISCONNECT))
# pragma unused(destinationName)
#endif // ! defined(MAM_DO_EXPLICIT_DISCONNECT)
    OD_LOG_ENTER();//####
    OD_LOG_P1("theChannel = ", theChannel);//####
    
    if (theChannel)
    {
#if defined(MAM_DO_EXPLICIT_DISCONNECT)
        if (! NetworkDisconnectWithRetries(theChannel->getName(), destinationName))
        {
            OD_LOG("(! NetworkDisconnectWithRetries(theChannel->getName(), destinationName))");//####
        }
#endif // defined(MAM_DO_EXPLICIT_DISCONNECT)
#if defined(MAM_DO_EXPLICIT_CLOSE)
        CloseChannel(*theChannel);
#endif // defined(MAM_DO_EXPLICIT_CLOSE)
        RelinquishChannel(theChannel);
    }
    OD_LOG_EXIT();//####
} // doDestroyTestChannel

/*! @brief Destroy a temporary channel that was used with a test.
 @param anEndpoint The endpoint to be connected to.
 @param theChannel A pointer to the temporary channel. */
static void doDestroyTestChannel(Endpoint & anEndpoint,
                                 Channel *  theChannel)
{
    doDestroyTestChannel(anEndpoint.getName(), theChannel);
} // doDestroyTestChannel

#if defined(__APPLE__)
# pragma mark *** Test Case 01 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestCreateEndpoint(const int argc,
                                char * *  argv) // create endpoint
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        Endpoint * stuff = doCreateEndpointForTest(argc, argv);
        
        if (stuff)
        {
            if (stuff->open())
            {
                OD_LOG_S1("endpoint name = ", stuff->getName().c_str());//####
                result = 0;
            }
            else
            {
                OD_LOG("! (stuff->open())");//####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestCreateEndpoint

#if defined(__APPLE__)
# pragma mark *** Test Case 02 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestConnectToEndpoint(const int argc,
                                   char * *  argv) // connect to endpoint
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        Endpoint * stuff = doCreateEndpointForTest(argc, argv);
        
        if (stuff)
        {
            EndpointStatusReporter reporter;
            
            if (stuff->open() && stuff->setReporter(reporter, true))
            {
                OD_LOG_S1("endpoint name = ", stuff->getName().c_str());//####
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomChannelName("test/connecttoendpoint_"));
                Channel *             outChannel = AcquireChannel();
                
                if (outChannel)
                {
                    if (OpenChannelWithRetries(*outChannel, aName))
                    {
                        outChannel->getReport(reporter);
                        if (AddOutputToChannelWithRetries(*outChannel, stuff->getName()))
                        {
                            result = 0;
#if defined(MAM_DO_EXPLICIT_DISCONNECT)
                            if (! NetworkDisconnectWithRetries(outChannel->getName(), stuff->getName()))
                            {
                                OD_LOG("(! NetworkDisconnectWithRetries(outChannel->getName(), "//####
                                       "stuff->getName()))");//####
                            }
#endif // defined(MAM_DO_EXPLICIT_DISCONNECT)
                        }
                        else
                        {
                            OD_LOG("! (AddOutputToChannelWithRetries(*outChannel, stuff->getName()))");//####
                        }
#if defined(MAM_DO_EXPLICIT_CLOSE)
                        CloseChannel(*outChannel);
#endif // defined(MAM_DO_EXPLICIT_CLOSE)
                    }
                    else
                    {
                        OD_LOG("! (OpenChannelWithRetries(*outChannel, aName))");//####
                    }
                    RelinquishChannel(outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");
                }
            }
            else
            {
                OD_LOG("! (stuff->open() && stuff->setReporter(reporter, true))");//####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestConnectToEndpoint

#if defined(__APPLE__)
# pragma mark *** Test Case 03 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestWriteToEndpoint(const int argc,
                                 char * *  argv) // send to endpoint
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        Endpoint * stuff = doCreateEndpointForTest(argc, argv);
        
        if (stuff)
        {
            EndpointStatusReporter reporter;
            Test03Handler          handler;
            
            if (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, true))
            {
                OD_LOG_S1("endpoint name = ", stuff->getName().c_str());//####
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomChannelName("test/writetoendpoint_"));
                Channel *             outChannel = AcquireChannel();
                
                if (outChannel)
                {
                    if (OpenChannelWithRetries(*outChannel, aName))
                    {
                        outChannel->getReport(reporter);
                        if (AddOutputToChannelWithRetries(*outChannel, stuff->getName()))
                        {
                            Package message;
                            
                            message.addString(aName);
                            message.addString("howdi");
                            if (outChannel->write(message))
                            {
                                result = 0;
#if defined(MAM_DO_EXPLICIT_DISCONNECT)
                                if (! NetworkDisconnectWithRetries(outChannel->getName(), stuff->getName()))
                                {
                                    OD_LOG("(! NetworkDisconnectWithRetries(outChannel->getName(), "//####
                                           "stuff->getName()))");//####
                                }
#endif // defined(MAM_DO_EXPLICIT_DISCONNECT)
                            }
                            else
                            {
                                OD_LOG("! (outChannel->write(message))");//####
                            }
                        }
                        else
                        {
                            OD_LOG("! (AddOutputToChannelWithRetries(*outChannel, stuff->getName()))");//####
                        }
#if defined(MAM_DO_EXPLICIT_CLOSE)
                        CloseChannel(*outChannel);
#endif // defined(MAM_DO_EXPLICIT_CLOSE)
                    }
                    else
                    {
                        OD_LOG("! (OpenChannelWithRetries(*outChannel, aName))");//####
                    }
                    RelinquishChannel(outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");
                }
            }
            else
            {
                OD_LOG("! (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, "//####
                          "true))");//####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestWriteToEndpoint

#if defined(__APPLE__)
# pragma mark *** Test Case 04 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestEchoFromEndpointWithReader(const int argc,
                                            char * *  argv) // send to endpoint
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        Endpoint * stuff = doCreateEndpointForTest(argc, argv);

        if (stuff)
        {
            EndpointStatusReporter reporter;
            Test04Handler          handler;
            
            if (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, true))
            {
                OD_LOG_S1("endpoint name = ", stuff->getName().c_str());//####
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomChannelName("test/echofromendpointwithreader_"));
                Channel *             outChannel = AcquireChannel();
                
                if (outChannel)
                {
                    if (OpenChannelWithRetries(*outChannel, aName))
                    {
                        outChannel->getReport(reporter);
                        if (AddOutputToChannelWithRetries(*outChannel, stuff->getName()))
                        {
                            Package message;
                            Package response;
                            
                            message.addString(aName);
                            message.addString("howdi");
                            if (outChannel->write(message, response))
                            {
//                                OD_LOG_S1("got ", response.toString().c_str());//####
                                result = 0;
#if defined(MAM_DO_EXPLICIT_DISCONNECT)
                                if (! NetworkDisconnectWithRetries(outChannel->getName(), stuff->getName()))
                                {
                                    OD_LOG("(! NetworkDisconnectWithRetries(outChannel->getName(), "//####
                                           "stuff->getName()))");//####
                                }
#endif // defined(MAM_DO_EXPLICIT_DISCONNECT)
                            }
                            else
                            {
                                OD_LOG("! (outChannel->write(message, response))");//####
                            }
                        }
                        else
                        {
                            OD_LOG("! (AddOutputToChannelWithRetries(*outChannel, stuff->getName()))");//####
                        }
#if defined(MAM_DO_EXPLICIT_CLOSE)
                        CloseChannel(*outChannel);
#endif // defined(MAM_DO_EXPLICIT_CLOSE)
                    }
                    else
                    {
                        OD_LOG("! (OpenChannelWithRetries(*outChannel, aName))");//####
                    }
                    RelinquishChannel(outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");
                }
            }
            else
            {
                OD_LOG("! (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, "//####
                          "true))");//####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestEchoFromEndpointWithReader

#if defined(__APPLE__)
# pragma mark *** Test Case 05 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestEchoFromEndpointWithReaderCreator(const int argc,
                                                   char * *  argv) // send to endpoint
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        Endpoint * stuff = doCreateEndpointForTest(argc, argv);

        if (stuff)
        {
            EndpointStatusReporter reporter;
            Test05HandlerCreator   handlerCreator;
            
            if (stuff->setInputHandlerCreator(handlerCreator) && stuff->open() && stuff->setReporter(reporter, true))
            {
                OD_LOG_S1("endpoint name = ", stuff->getName().c_str());//####
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomChannelName("test/echofromendpointwithreadercreator_"));
                Channel *             outChannel = AcquireChannel();
                
                if (outChannel)
                {
                    if (OpenChannelWithRetries(*outChannel, aName))
                    {
                        outChannel->getReport(reporter);
                        if (AddOutputToChannelWithRetries(*outChannel, stuff->getName()))
                        {
                            Package message;
                            Package response;
                            
                            message.addString(aName);
                            message.addString("howdi");
                            if (outChannel->write(message, response))
                            {
//                                OD_LOG_S1("got ", response.toString().c_str());//####
                                result = 0;
#if defined(MAM_DO_EXPLICIT_DISCONNECT)
                                if (! NetworkDisconnectWithRetries(outChannel->getName(), stuff->getName()))
                                {
                                    OD_LOG("(! NetworkDisconnectWithRetries(outChannel->getName(), "//####
                                           "stuff->getName()))");//####
                                }
#endif // defined(MAM_DO_EXPLICIT_DISCONNECT)
                            }
                            else
                            {
                                OD_LOG("! (outChannel->write(message, response))");//####
                            }
                        }
                        else
                        {
                            OD_LOG("! (AddOutputToChannelWithRetries(*outChannel, stuff->getName()))");//####
                        }
#if defined(MAM_DO_EXPLICIT_CLOSE)
                        CloseChannel(*outChannel);
#endif // defined(MAM_DO_EXPLICIT_CLOSE)
                    }
                    else
                    {
                        OD_LOG("! (OpenChannelWithRetries(*outChannel, aName))");//####
                    }
                    RelinquishChannel(outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");
                }
            }
            else
            {
                OD_LOG("! (stuff->setInputHandlerCreator(handlerCreator) && stuff->open() && "//####
                          "stuff->setReporter(reporter, true))");//####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestEchoFromEndpointWithReaderCreator

#if defined(__APPLE__)
# pragma mark *** Test Case 06 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestCreateRequest(const int argc,
                               char * *  argv) // create request
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        if (0 == argc)
        {
            OD_LOG("0 == argc");//####
        }
        else
        {
            Package parameters;
            
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
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestCreateRequest

#if defined(__APPLE__)
# pragma mark *** Test Case 07 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestCreateResponse(const int argc,
                                char * *  argv) // create request
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        Package parameters;
        
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
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestCreateResponse

#if defined(__APPLE__)
# pragma mark *** Test Case 08 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestRequestEchoFromEndpoint(const int argc,
                                         char * *  argv) // create request
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        Endpoint * stuff = doCreateEndpointForTest(argc, argv);
        
        if (stuff)
        {
            EndpointStatusReporter reporter;
            Test08Handler          handler;
            
            if (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, true))
            {
                Channel * outChannel = doCreateTestChannel(stuff->getName(), "test/requestechofromendpoint_");
                
                if (outChannel)
                {
                    OD_LOG_S1("endpoint name = ", stuff->getName().c_str());//####
                    Package         parameters("some to send");
                    ServiceRequest  request(MAM_ECHO_REQUEST, parameters);
                    ServiceResponse response;
                    
                    if (request.send(*outChannel, &response))
                    {
                        OD_LOG_LL1("response size = ", response.count());//####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_LOG_S1("response value = ", response.element(ii).toString().c_str());//####
                        }
                        result = 0;
                    }
                    else
                    {
                        OD_LOG("! (request.send(*outChannel, &response))");//####
                    }
                    doDestroyTestChannel(stuff->getName(), outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");//####
                }
            }
            else
            {
                OD_LOG("! (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter("//####
                          "reporter, true))");//####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestRequestEchoFromEndpoint

#if defined(__APPLE__)
# pragma mark *** Test Case 09 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestRequestEchoFromServiceUsingDefaultWithReader(const int argc,
                                                              char * *  argv) // send 'echo' request
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        Test09Service * stuff = new Test09Service(argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                Channel * outChannel = doCreateTestChannel(stuff->getEndpoint(),
                                                           "test/requestechofromserviceusingdefaultwithreader");
                
                if (outChannel)
                {
                    Package         parameters("some to send");
                    ServiceRequest  request(MAM_ECHO_REQUEST, parameters);
                    ServiceResponse response;
                    
                    if (request.send(*outChannel, &response))
                    {
                        OD_LOG_LL1("response size = ", response.count());//####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_LOG_S1("response value = ", response.element(ii).toString().c_str());//####
                        }
                        result = 0;
                    }
                    else
                    {
                        OD_LOG("! (request.send(*outChannel, &response))");//####
                    }
                    doDestroyTestChannel(stuff->getEndpoint(), outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");//####
                }
                stuff->stop();
            }
            else
            {
                OD_LOG("! (stuff->start())");//####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestRequestEchoFromServiceUsingDefaultWithReader

#if defined(__APPLE__)
# pragma mark *** Test Case 10 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestRequestEchoFromServiceUsingDefaultWithReaderCreator(const int argc,
                                                                     char * *  argv) // send 'echo' request
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        Test10Service * stuff = new Test10Service(argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                Channel * outChannel = doCreateTestChannel(stuff->getEndpoint(),
                                                           "test/requestechofromserviceusingdefaultwithreadercreator_");
                
                if (outChannel)
                {
                    Package         parameters("some to send");
                    ServiceRequest  request(MAM_ECHO_REQUEST, parameters);
                    ServiceResponse response;
                    
                    if (request.send(*outChannel, &response))
                    {
                        OD_LOG_LL1("response size = ", response.count());//####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_LOG_S1("response value = ", response.element(ii).toString().c_str());//####
                        }
                        result = 0;
                    }
                    else
                    {
                        OD_LOG("! (request.send(*outChannel, &response))");//####
                    }
                    doDestroyTestChannel(stuff->getEndpoint(), outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");//####
                }
                stuff->stop();
            }
            else
            {
                OD_LOG("! (stuff->start())");//####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestRequestEchoFromServiceUsingDefaultWithReaderCreator

#if defined(__APPLE__)
# pragma mark *** Test Case 11 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestRequestEchoFromServiceWithRequestHandler(const int argc,
                                                          char * *  argv) // create 'echo' request
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        Test11Service * stuff = new Test11Service(argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                Channel * outChannel = doCreateTestChannel(stuff->getEndpoint(),
                                                           "test/requestechofromservicewithrequesthandler_");
                
                if (outChannel)
                {
                    Package         parameters("some to send");
                    ServiceRequest  request(MAM_ECHO_REQUEST, parameters);
                    ServiceResponse response;
                    
                    if (request.send(*outChannel, &response))
                    {
                        if (3 == response.count())
                        {
                            yarp::os::ConstString expected[] = { "some", "to", "send" };
                            
                            result = 0;
                            for (int ii = 0; (! result) && (ii < response.count()); ++ii)
                            {
                                if (expected[ii] != response.element(ii).toString())
                                {
                                    OD_LOG_S2("expected[ii] = ", expected[ii].c_str(),//####
                                              "response.element(ii).toString() = ",//####
                                              response.element(ii).toString().c_str());//####
                                    result = 1;
                                }
                            }
                        }
                        else
                        {
                            OD_LOG("! (3 == response.count())");//####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request.send(*outChannel, &response))");//####
                    }
                    doDestroyTestChannel(stuff->getEndpoint(), outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");//####
                }
                stuff->stop();
            }
            else
            {
                OD_LOG("! (stuff->start())");//####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestRequestEchoFromServiceWithRequestHandler

#if defined(__APPLE__)
# pragma mark *** Test Case 12 ***
#endif // defined(__APPLE__)

/*! @brief Check the response from the 'list' request for this test.
 @param response The response to be analyzed.
 @returns @c true if the expected values are all present and @c false if they are not or if unexpected values appear. */
static bool checkResponseFromEchoFromServiceWithRequestHandlerAndInfo(const ServiceResponse & response)
{
    OD_LOG_ENTER();//####
    bool result = false;
    
    try
    {
        if (3 <= response.count())
        {
            bool sawEcho = false;
            bool sawInfo = false;
            bool sawList = false;
            
            result = true;
            for (int ii = 0; result && (ii < response.count()); ++ii)
            {
                yarp::os::Value anElement(response.element(ii));
                
                if (anElement.isDict())
                {
                    yarp::os::Property * asDict = anElement.asDict();
                    bool                 hasInput = asDict->check(MAM_REQREP_DICT_INPUT_KEY);
                    bool                 hasOutput = asDict->check(MAM_REQREP_DICT_OUTPUT_KEY);
                    
                    if (asDict->check(MAM_REQREP_DICT_REQUEST_KEY))
                    {
                        yarp::os::ConstString aName(asDict->find(MAM_REQREP_DICT_REQUEST_KEY).asString());
                        
                        if (aName == MAM_LIST_REQUEST)
                        {
                            if (sawList)
                            {
                                result = false;
                            }
                            else if ((! hasInput) && hasOutput)
                            {
                                yarp::os::ConstString itsOutput(asDict->find(MAM_REQREP_DICT_OUTPUT_KEY).asString());
                                
                                sawList = (itsOutput == "([]+)");
                            }
                        }
                        else if (aName == MAM_INFO_REQUEST)
                        {
                            if (sawInfo)
                            {
                                result = false;
                            }
                            else if (hasInput && hasOutput)
                            {
                                yarp::os::ConstString itsOutput(asDict->find(MAM_REQREP_DICT_OUTPUT_KEY).asString());
                                yarp::os::ConstString itsInput(asDict->find(MAM_REQREP_DICT_INPUT_KEY).asString());
                                
                                sawInfo = ((itsInput == ".+") && (itsOutput == "([]?)"));
                            }
                        }
                        else if (aName == MAM_ECHO_REQUEST)
                        {
                            if (sawEcho)
                            {
                                result = false;
                            }
                            else if (hasInput && hasOutput)
                            {
                                yarp::os::ConstString itsOutput(asDict->find(MAM_REQREP_DICT_OUTPUT_KEY).asString());
                                yarp::os::ConstString itsInput(asDict->find(MAM_REQREP_DICT_INPUT_KEY).asString());
                                
                                sawEcho = ((itsInput == ".*") && (itsOutput == ".*"));
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
            result &= (sawInfo && sawEcho && sawList);
        }
        else
        {
            // Wrong number of values in the response.
            OD_LOG("! (3 <= response.count())");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_B(result);//####
    return result;
} // checkResponseFromEchoFromServiceWithRequestHandlerAndInfo

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestRequestEchoFromServiceWithRequestHandlerAndInfo(const int argc,
                                                                 char * *  argv) // send 'list' request
{
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        Test12Service * stuff = new Test12Service(argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                Channel * outChannel = doCreateTestChannel(stuff->getEndpoint(),
                                                           "test/requestechofromservicewithrequesthandlerandinfo_");
                
                if (outChannel)
                {
                    ServiceRequest  request(MAM_LIST_REQUEST);
                    ServiceResponse response;
                    
                    if (request.send(*outChannel, &response))
                    {
                        OD_LOG_LL1("response size = ", response.count());//####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_LOG_S1("response value = ", response.element(ii).toString().c_str());//####
                        }
                        if (checkResponseFromEchoFromServiceWithRequestHandlerAndInfo(response))
                        {
                            result = 0;
                        }
                        else
                        {
                            OD_LOG("! (checkResponseFromEchoFromServiceWithRequestHandlerAndInfo(response))");//####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request.send(*outChannel, &response))");//####
                    }
                        doDestroyTestChannel(stuff->getEndpoint(), outChannel);
                }
                else
                {
                    OD_LOG("! (outChannel)");//####
                }
                stuff->stop();
            }
            else
            {
                OD_LOG("! (stuff->start())");//####
            }
            delete stuff;
        }
        else
        {
            OD_LOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestRequestEchoFromServiceWithRequestHandlerAndInfo

#if (defined(__APPLE__) || defined(__linux__))
/*! @brief The signal handler to catch requests to stop the service.
 @param signal The signal being handled. */
static void catchSignal(int signal)
{
# if (! defined(OD_ENABLE_LOGGING))
#  pragma unused(signal)
# endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_ENTER();//####
    OD_LOG_LL1("signal = ", signal);//####
    OD_LOG_EXIT_EXIT(1);//####
    yarp::os::exit(1);
} // catchSignal
#endif // defined(__APPLE__) || defined(__linux__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for unit tests.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the unit tests.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID |//####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr);//####
    OD_LOG_ENTER();//####
    int result = 1;

    try
    {
        if (yarp::os::Network::checkNetwork())
        {
            yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
            
            MoAndMe::Initialize();
            if (0 < --argc)
            {
                int selector = atoi(argv[1]);
                
#if (defined(__APPLE__) || defined(__linux__))
                MoAndMe::SetSignalHandlers(catchSignal);
#endif // defined(__APPLE__) || defined(__linux__)
                switch (selector)
                {
                    case 0:
                        // Just used to validate the random number seed.
                        result = 0;
                        break;
                        
                    case 1:
                        result = doTestCreateEndpoint(argc - 1, argv + 2);
                        break;
                        
                    case 2:
                        result = doTestConnectToEndpoint(argc - 1, argv + 2);
                        break;
                        
                    case 3:
                        result = doTestWriteToEndpoint(argc - 1, argv + 2);
                        break;
                        
                    case 4:
                        result = doTestEchoFromEndpointWithReader(argc - 1, argv + 2);
                        break;
                        
                    case 5:
                        result = doTestEchoFromEndpointWithReaderCreator(argc - 1, argv + 2);
                        break;
                        
                    case 6:
                        result = doTestCreateRequest(argc - 1, argv + 2);
                        break;
                        
                    case 7:
                        result = doTestCreateResponse(argc - 1, argv + 2);
                        break;
                        
                    case 8:
                        result = doTestRequestEchoFromEndpoint(argc - 1, argv + 2);
                        break;
                        
                    case 9:
                        result = doTestRequestEchoFromServiceUsingDefaultWithReader(argc - 1, argv + 2);
                        break;
                        
                    case 10:
                        result = doTestRequestEchoFromServiceUsingDefaultWithReaderCreator(argc - 1, argv + 2);
                        break;
                        
                    case 11:
                        result = doTestRequestEchoFromServiceWithRequestHandler(argc - 1, argv + 2);
                        break;
                        
                    case 12:
                        result = doTestRequestEchoFromServiceWithRequestHandlerAndInfo(argc - 1, argv + 2);
                        break;
                        
                    default:
                        break;
                        
                }
                if (result)
                {
                    OD_LOG_LL1("%%%%%%% unit test failure = ", result);//####
                }
            }
            else
            {
                OD_LOG("! (0 < --argc)");//####
            }
        }
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork())");//####
            cerr << "YARP network not running." << endl;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(result);//####
    return result;
} // main