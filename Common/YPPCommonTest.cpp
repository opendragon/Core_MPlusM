//--------------------------------------------------------------------------------------
//
//  File:       YPPCommonTest.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The test driver for the unit tests of the Yarp++ common library.
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

//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "CommonTests/YPPTEndpointStatusReporter.h"
#include "CommonTests/YPPTTest03Handler.h"
#include "CommonTests/YPPTTest04Handler.h"
#include "CommonTests/YPPTTest05HandlerCreator.h"
#include "CommonTests/YPPTTest08Handler.h"
#include "CommonTests/YPPTTest09Service.h"
#include "CommonTests/YPPTTest10Service.h"
#include "CommonTests/YPPTTest11Service.h"
#include "CommonTests/YPPTTest12Service.h"
#include "YPPBaseClient.h"
#include "YPPBaseRequestHandler.h"
#include "YPPEndpoint.h"
#include "YPPRequests.h"
#include "YPPServiceRequest.h"
#include <iostream>
#include <yarp/os/all.h>

using namespace YarpPlusPlus;
using namespace YarpPlusPlusTest;
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
                                          char **   argv)
{
    OD_SYSLOG_ENTER();//####
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
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_P(stuff);//####
    return stuff;
} // doCreateEndpointForTest

/*! @brief Create a temporary port for a test.
 @param destinationName The name of the port to be connected to.
 @param portPath The root path for the new temporary port.
 @returns A pointer to a newly-allocated temporary port. */
static yarp::os::Port * doCreateTestPort(const yarp::os::ConstString & destinationName,
                                         const char *                  portPath)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S2("destinationName = ", destinationName.c_str(), "portPath = ", portPath);//####
    yarp::os::ConstString aName(GetRandomPortName(portPath));
    yarp::os::Port *      newPort = new yarp::os::Port();
    
    if (newPort)
    {
        if (newPort->open(aName))
        {
            if (! yarp::os::Network::connect(aName, destinationName))
            {
                OD_SYSLOG("(! yarp::os::Network::connect(aName, destinationName))");//####
                newPort->close();
                delete newPort;
                newPort = NULL;
            }
        }
        else
        {
            OD_SYSLOG("! (newPort->open(portPath))");//####
        }
    }
    else
    {
        OD_SYSLOG("! (newPort)");//####
    }
    OD_SYSLOG_EXIT_P(newPort);//####
    return newPort;
} // doCreateTestPort

/*! @brief Create a temporary port for a test.
 @param anEndpoint The endpoint to be connected to.
 @param portPath The root path for the new temporary port.
 @returns A pointer to a newly-allocated temporary port. */
static yarp::os::Port * doCreateTestPort(Endpoint &   anEndpoint,
                                         const char * portPath)
{
    return doCreateTestPort(anEndpoint.getName(), portPath);
} // doCreateTestPort

/*! @brief Destroy a temporary port that was used with a test.
 @param destinationName The name of the port that the temporary port was connected to.
 @param thePort A pointer to the temporary port. */
static void doDestroyTestPort(const yarp::os::ConstString & destinationName,
                              yarp::os::Port *              thePort)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("thePort = ", thePort);//####
    
    if (thePort)
    {
        if (! yarp::os::Network::disconnect(thePort->getName(), destinationName))
        {
            OD_SYSLOG("(! yarp::os::Network::disconnect(thePort->getName(), destinationName))");//####
        }
        thePort->close();
        delete thePort;
    }
    OD_SYSLOG_EXIT();//####
} // doDestroyTestPort

/*! @brief Destroy a temporary port that was used with a test.
 @param anEndpoint The endpoint to be connected to.
 @param thePort A pointer to the temporary port. */
static void doDestroyTestPort(Endpoint &       anEndpoint,
                              yarp::os::Port * thePort)
{
    doDestroyTestPort(anEndpoint.getName(), thePort);
} // doDestroyTestPort

#if defined(__APPLE__)
# pragma mark *** Test Case 01 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase01(const int argc,
                    char **   argv) // create endpoint
{
    OD_SYSLOG_ENTER();//####
    int result = 1;
    
    try
    {
        Endpoint * stuff = doCreateEndpointForTest(argc, argv);
        
        if (stuff)
        {
            if (stuff->open())
            {
                OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
                result = 0;
            }
            else
            {
                OD_SYSLOG("! (stuff->open())");//####
            }
            delete stuff;
        }
        else
        {
            OD_SYSLOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase01

#if defined(__APPLE__)
# pragma mark *** Test Case 02 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase02(const int argc,
                    char **   argv) // connect to endpoint
{
    OD_SYSLOG_ENTER();//####
    int result = 1;
    
    try
    {
        Endpoint * stuff = doCreateEndpointForTest(argc, argv);
        
        if (stuff)
        {
            EndpointStatusReporter reporter;
            
            if (stuff->open() && stuff->setReporter(reporter, true))
            {
                OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomPortName("test/case02_"));
                yarp::os::Port *      outPort = new yarp::os::Port();
                
                if (outPort)
                {
                    OD_SYSLOG_S1("opening ", aName.c_str());//####
                    if (outPort->open(aName))
                    {
                        OD_SYSLOG("(outPort->open(aName))");//####
                        outPort->getReport(reporter);
                        if (outPort->addOutput(stuff->getName()))
                        {
                            OD_SYSLOG("(outPort->addOutput(stuff->getName()))");//####
                            result = 0;
                        }
                        else
                        {
                            OD_SYSLOG("! (outPort->addOutput(stuff->getName()))");//####
                        }
                        OD_SYSLOG_S1("about to close, port = ", aName.c_str());//####
                        outPort->close();
                        OD_SYSLOG("close completed.");//####
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort->open(aName))");//####
                    }
                    delete outPort;
                }
                else
                {
                    OD_SYSLOG("! (outPort)");
                }
            }
            else
            {
                OD_SYSLOG("! (stuff->open() && stuff->setReporter(reporter, true))");//####
            }
            delete stuff;
        }
        else
        {
            OD_SYSLOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase02

#if defined(__APPLE__)
# pragma mark *** Test Case 03 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase03(const int argc,
                    char **   argv) // send to endpoint
{
    OD_SYSLOG_ENTER();//####
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
                OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomPortName("test/case03_"));
                yarp::os::Port *      outPort = new yarp::os::Port();
                
                if (outPort)
                {
                    OD_SYSLOG_S1("opening ", aName.c_str());//####
                    if (outPort->open(aName))
                    {
                        OD_SYSLOG("(outPort->open(aName))");//####
                        outPort->getReport(reporter);
                        if (outPort->addOutput(stuff->getName()))
                        {
                            OD_SYSLOG("(outPort->addOutput(stuff->getName()))");//####
                            yarp::os::Bottle message;
                            
                            message.addString(aName);
                            message.addString("howdi");
                            if (outPort->write(message))
                            {
                                OD_SYSLOG("(outPort->write(message))");//####
                                result = 0;
                            }
                            else
                            {
                                OD_SYSLOG("! (outPort->write(message))");//####
                            }
                        }
                        else
                        {
                            OD_SYSLOG("! (outPort->addOutput(stuff->getName()))");//####
                        }
                        OD_SYSLOG_S1("about to close, port = ", aName.c_str());//####
                        outPort->close();
                        OD_SYSLOG("close completed.");//####
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort->open(aName))");//####
                    }
                    delete outPort;
                }
                else
                {
                    OD_SYSLOG("! (outPort)");
                }
            }
            else
            {
                OD_SYSLOG("! (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, "//####
                          "true))");//####
            }
            delete stuff;
        }
        else
        {
            OD_SYSLOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase03

#if defined(__APPLE__)
# pragma mark *** Test Case 04 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase04(const int argc,
                    char **   argv) // send to endpoint
{
    OD_SYSLOG_ENTER();//####
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
                OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomPortName("test/case04_"));
                yarp::os::Port *      outPort = new yarp::os::Port();
                
                if (outPort)
                {
                    OD_SYSLOG_S1("opening ", aName.c_str());//####
                    if (outPort->open(aName))
                    {
                        OD_SYSLOG("(outPort->open(aName))");//####
                        outPort->getReport(reporter);
                        if (outPort->addOutput(stuff->getName()))
                        {
                            OD_SYSLOG("(outPort->addOutput(stuff->getName()))");//####
                            yarp::os::Bottle message;
                            yarp::os::Bottle response;
                            
                            message.addString(aName);
                            message.addString("howdi");
                            if (outPort->write(message, response))
                            {
                                OD_SYSLOG("(outPort->write(message, response))");//####
                                OD_SYSLOG_S1("got ", response.toString().c_str());//####
                                result = 0;
                            }
                            else
                            {
                                OD_SYSLOG("! (outPort->write(message, response))");//####
                            }
                        }
                        else
                        {
                            OD_SYSLOG("! (outPort->addOutput(stuff->getName()))");//####
                        }
                        OD_SYSLOG_S1("about to close, port = ", aName.c_str());//####
                        outPort->close();
                        OD_SYSLOG("close completed.");//####
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort->open(aName))");//####
                    }
                    delete outPort;
                }
                else
                {
                    OD_SYSLOG("! (outPort)");
                }
            }
            else
            {
                OD_SYSLOG("! (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, "//####
                          "true))");//####
            }
            delete stuff;
        }
        else
        {
            OD_SYSLOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase04

#if defined(__APPLE__)
# pragma mark *** Test Case 05 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase05(const int argc,
                    char **   argv) // send to endpoint
{
    OD_SYSLOG_ENTER();//####
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
                OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
                // Now we try to connect!
                yarp::os::ConstString aName(GetRandomPortName("test/case05_"));
                yarp::os::Port *      outPort = new yarp::os::Port();
                
                if (outPort)
                {
                    OD_SYSLOG_S1("opening ", aName.c_str());//####
                    if (outPort->open(aName))
                    {
                        OD_SYSLOG("(outPort->open(aName))");//####
                        outPort->getReport(reporter);
                        if (outPort->addOutput(stuff->getName()))
                        {
                            OD_SYSLOG("(outPort->addOutput(stuff->getName()))");//####
                            yarp::os::Bottle message;
                            yarp::os::Bottle response;
                            
                            message.addString(aName);
                            message.addString("howdi");
                            if (outPort->write(message, response))
                            {
                                OD_SYSLOG("(outPort->write(message, response))");//####
                                OD_SYSLOG_S1("got ", response.toString().c_str());//####
                                result = 0;
                            }
                            else
                            {
                                OD_SYSLOG("! (outPort->write(message, response))");//####
                            }
                        }
                        else
                        {
                            OD_SYSLOG("! (outPort->addOutput(stuff->getName()))");//####
                        }
                        OD_SYSLOG_S1("about to close, port = ", aName.c_str());//####
                        outPort->close();
                        OD_SYSLOG("close completed.");//####
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort->open(aName))");//####
                    }
                    delete outPort;
                }
                else
                {
                    OD_SYSLOG("! (outPort)");
                }
            }
            else
            {
                OD_SYSLOG("! (stuff->setInputHandlerCreator(handlerCreator) && stuff->open() && "//####
                          "stuff->setReporter(reporter, true))");//####
            }
            delete stuff;
        }
        else
        {
            OD_SYSLOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase05

#if defined(__APPLE__)
# pragma mark *** Test Case 06 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase06(const int argc,
                    char **   argv) // create request
{
    OD_SYSLOG_ENTER();//####
    int result = 1;
    
    try
    {
        if (0 == argc)
        {
            OD_SYSLOG("0 == argc");//####
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
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase06

#if defined(__APPLE__)
# pragma mark *** Test Case 07 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase07(const int argc,
                    char **   argv) // create request
{
    OD_SYSLOG_ENTER();//####
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
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase07

#if defined(__APPLE__)
# pragma mark *** Test Case 08 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase08(const int argc,
                    char **   argv) // create request
{
    OD_SYSLOG_ENTER();//####
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
                yarp::os::Port * outPort = doCreateTestPort(stuff->getName(), "test/case08_");
                
                if (outPort)
                {
                    OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(YPP_ECHO_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*outPort, &response))
                    {
                        OD_SYSLOG_LL1("response size = ", response.count());//####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_SYSLOG_S1("response value = ", response.element(ii).toString().c_str());//####
                        }
                        result = 0;
                    }
                    else
                    {
                        OD_SYSLOG("! (request.send(*outPort, &response))");//####
                    }
                    doDestroyTestPort(stuff->getName(), outPort);
                }
                else
                {
                    OD_SYSLOG("! (outPort)");//####
                }
            }
            else
            {
                OD_SYSLOG("! (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter("//####
                          "reporter, true))");//####
            }
            delete stuff;
        }
        else
        {
            OD_SYSLOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase08

#if defined(__APPLE__)
# pragma mark *** Test Case 09 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase09(const int argc,
                    char **   argv) // send 'echo' request
{
    OD_SYSLOG_ENTER();//####
    int result = 1;
    
    try
    {
        Test09Service * stuff = new Test09Service(argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                yarp::os::Port * outPort = doCreateTestPort(stuff->getEndpoint(), "test/case09_");
                
                if (outPort)
                {
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(YPP_ECHO_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*outPort, &response))
                    {
                        OD_SYSLOG_LL1("response size = ", response.count());//####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_SYSLOG_S1("response value = ", response.element(ii).toString().c_str());//####
                        }
                        result = 0;
                    }
                    else
                    {
                        OD_SYSLOG("! (request.send(*outPort, &response))");//####
                    }
                    doDestroyTestPort(stuff->getEndpoint(), outPort);
                }
                else
                {
                    OD_SYSLOG("! (outPort)");//####
                }
                stuff->stop();
            }
            else
            {
                OD_SYSLOG("! (stuff->start())");//####
            }
            delete stuff;
        }
        else
        {
            OD_SYSLOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase09

#if defined(__APPLE__)
# pragma mark *** Test Case 10 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase10(const int argc,
                    char **   argv) // send 'echo' request
{
    OD_SYSLOG_ENTER();//####
    int result = 1;
    
    try
    {
        Test10Service * stuff = new Test10Service(argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                yarp::os::Port * outPort = doCreateTestPort(stuff->getEndpoint(), "test/case10_");
                
                if (outPort)
                {
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(YPP_ECHO_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*outPort, &response))
                    {
                        OD_SYSLOG_LL1("response size = ", response.count());//####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_SYSLOG_S1("response value = ", response.element(ii).toString().c_str());//####
                        }
                        result = 0;
                    }
                    else
                    {
                        OD_SYSLOG("! (request.send(*outPort, &response))");//####
                    }
                    doDestroyTestPort(stuff->getEndpoint(), outPort);
                }
                else
                {
                    OD_SYSLOG("! (outPort)");//####
                }
                stuff->stop();
            }
            else
            {
                OD_SYSLOG("! (stuff->start())");//####
            }
            delete stuff;
        }
        else
        {
            OD_SYSLOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase10

#if defined(__APPLE__)
# pragma mark *** Test Case 11 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase11(const int argc,
                    char **   argv) // create 'echo' request
{
    OD_SYSLOG_ENTER();//####
    int result = 1;
    
    try
    {
        Test11Service * stuff = new Test11Service(argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                yarp::os::Port * outPort = doCreateTestPort(stuff->getEndpoint(), "test/case11_");
                
                if (outPort)
                {
                    yarp::os::Bottle parameters("some to send");
                    ServiceRequest   request(YPP_ECHO_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*outPort, &response))
                    {
                        if (3 == response.count())
                        {
                            yarp::os::ConstString expected[] = { "some", "to", "send" };
                            
                            result = 0;
                            for (int ii = 0; (! result) && (ii < response.count()); ++ii)
                            {
                                if (expected[ii] != response.element(ii).toString())
                                {
                                    OD_SYSLOG_S2("expected[ii] = ", expected[ii].c_str(),//####
                                                 "response.element(ii).toString() = ",//####
                                                 response.element(ii).toString().c_str());//####
                                    result = 1;
                                }
                            }
                        }
                        else
                        {
                            OD_SYSLOG("! (3 == response.count())");//####
                        }
                    }
                    else
                    {
                        OD_SYSLOG("! (request.send(*outPort, &response))");//####
                    }
                    doDestroyTestPort(stuff->getEndpoint(), outPort);
                }
                else
                {
                    OD_SYSLOG("! (outPort)");//####
                }
                stuff->stop();
            }
            else
            {
                OD_SYSLOG("! (stuff->start())");//####
            }
            delete stuff;
        }
        else
        {
            OD_SYSLOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase11

#if defined(__APPLE__)
# pragma mark *** Test Case 12 ***
#endif // defined(__APPLE__)

/*! @brief Check the response from the 'list' request for this test.
 @param response The response to be analyzed.
 @returns @c true if the expected values are all present and @c false if they are not or if unexpected values appear. */
static bool checkList12Response(const ServiceResponse & response)
{
    OD_SYSLOG_ENTER();//####
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
                    bool                 hasInput = asDict->check(YPP_REQREP_DICT_INPUT_KEY);
                    bool                 hasOutput = asDict->check(YPP_REQREP_DICT_OUTPUT_KEY);
                    
                    if (asDict->check(YPP_REQREP_DICT_REQUEST_KEY))
                    {
                        yarp::os::ConstString aName(asDict->find(YPP_REQREP_DICT_REQUEST_KEY).asString());
                        
                        if (aName == YPP_LIST_REQUEST)
                        {
                            if (sawList)
                            {
                                result = false;
                            }
                            else if ((! hasInput) && hasOutput)
                            {
                                yarp::os::ConstString itsOutput(asDict->find(YPP_REQREP_DICT_OUTPUT_KEY).asString());
                                
                                sawList = (itsOutput == "([]+)");
                            }
                        }
                        else if (aName == YPP_INFO_REQUEST)
                        {
                            if (sawInfo)
                            {
                                result = false;
                            }
                            else if (hasInput && hasOutput)
                            {
                                yarp::os::ConstString itsOutput(asDict->find(YPP_REQREP_DICT_OUTPUT_KEY).asString());
                                yarp::os::ConstString itsInput(asDict->find(YPP_REQREP_DICT_INPUT_KEY).asString());
                                
                                sawInfo = ((itsInput == ".+") && (itsOutput == "([]?)"));
                            }
                        }
                        else if (aName == YPP_ECHO_REQUEST)
                        {
                            if (sawEcho)
                            {
                                result = false;
                            }
                            else if (hasInput && hasOutput)
                            {
                                yarp::os::ConstString itsOutput(asDict->find(YPP_REQREP_DICT_OUTPUT_KEY).asString());
                                yarp::os::ConstString itsInput(asDict->find(YPP_REQREP_DICT_INPUT_KEY).asString());
                                
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
            OD_SYSLOG("! (3 <= response.count())");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // checkList12Response

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase12(const int argc,
                    char **   argv) // send 'list' request
{
    OD_SYSLOG_ENTER();//####
    int result = 1;
    
    try
    {
        Test12Service * stuff = new Test12Service(argc, argv);
        
        if (stuff)
        {
            if (stuff->start())
            {
                yarp::os::Port * outPort = doCreateTestPort(stuff->getEndpoint(), "test/case12_");
                
                if (outPort)
                {
                    ServiceRequest  request(YPP_LIST_REQUEST);
                    ServiceResponse response;
                    
                    if (request.send(*outPort, &response))
                    {
                        OD_SYSLOG_LL1("response size = ", response.count());//####
                        for (int ii = 0; ii < response.count(); ++ii)
                        {
                            OD_SYSLOG_S1("response value = ", response.element(ii).toString().c_str());//####
                        }
                        if (checkList12Response(response))
                        {
                            result = 0;
                        }
                        else
                        {
                            OD_SYSLOG("! (checkList12Response(response))");//####
                        }
                    }
                    else
                    {
                        OD_SYSLOG("! (request.send(*outPort, &response))");//####
                    }
                        doDestroyTestPort(stuff->getEndpoint(), outPort);
                }
                else
                {
                    OD_SYSLOG("! (outPort)");//####
                }
                stuff->stop();
            }
            else
            {
                OD_SYSLOG("! (stuff->start())");//####
            }
            delete stuff;
        }
        else
        {
            OD_SYSLOG("! (stuff)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase12

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for unit tests.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the unit tests.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID |//####
                   kODSyslogOptionEnableThreadSupport | kODSyslogOptionWriteToStderr);//####
    OD_SYSLOG_ENTER();//####
    int result = 1;

    try
    {
        if (yarp::os::Network::checkNetwork())
        {
#if (defined(ENABLE_OD_SYSLOG) && defined(DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network::setVerbosity(1);
#else // ! (defined(ENABLE_OD_SYSLOG) && defined(DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network::setVerbosity(-1);
#endif // ! (defined(ENABLE_OD_SYSLOG) && defined(DEBUG_INCLUDES_YARP_TRACE))
            yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
            
            YarpPlusPlus::Initialize();
            if (0 < --argc)
            {
                int selector = atoi(argv[1]);
                
                switch (selector)
                {
                    case 0:
                        // Just used to validate the random number seed.
                        result = 0;
                        break;
                        
                    case 1:
                        result = doCase01(argc - 1, argv + 2);
                        break;
                        
                    case 2:
                        result = doCase02(argc - 1, argv + 2);
                        break;
                        
                    case 3:
                        result = doCase03(argc - 1, argv + 2);
                        break;
                        
                    case 4:
                        result = doCase04(argc - 1, argv + 2);
                        break;
                        
                    case 5:
                        result = doCase05(argc - 1, argv + 2);
                        break;
                        
                    case 6:
                        result = doCase06(argc - 1, argv + 2);
                        break;
                        
                    case 7:
                        result = doCase07(argc - 1, argv + 2);
                        break;
                        
                    case 8:
                        result = doCase08(argc - 1, argv + 2);
                        break;
                        
                    case 9:
                        result = doCase09(argc - 1, argv + 2);
                        break;
                        
                    case 10:
                        result = doCase10(argc - 1, argv + 2);
                        break;
                        
                    case 11:
                        result = doCase11(argc - 1, argv + 2);
                        break;
                        
                    case 12:
                        result = doCase12(argc - 1, argv + 2);
                        break;
                        
                    default:
                        break;
                        
                }
            }
            else
            {
                OD_SYSLOG("! (0 < --argc)");//####
            }
        }
        else
        {
            OD_SYSLOG("! (yarp::os::Network::checkNetwork())");//####
            cerr << "YARP network not running." << endl;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
    }
    yarp::os::Network::fini();
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // main
