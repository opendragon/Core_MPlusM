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
//  Copyright:  (c) 2014 by OpenDragon.
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
#include "Tests/YPPTEndpointStatusReporter.h"
#include "Tests/YPPTTest03Handler.h"
#include "Tests/YPPTTest04Handler.h"
#include "Tests/YPPTTest05HandlerCreator.h"
#include "Tests/YPPTTest08Handler.h"
#include "Tests/YPPTTest09Service.h"
#include "Tests/YPPTTest10Service.h"
#include "Tests/YPPTTest11Service.h"
#include "Tests/YPPTTest12Service.h"
#include "Tests/YPPTTest14Service.h"
#include "Tests/YPPTTest15Service.h"
#include "Tests/YPPTTest16Service.h"
#include "YPPBaseClient.h"
#include "YPPBaseRequestHandler.h"
#include "YPPEndpoint.h"
#include "YPPRegistryService.h"
#include "YPPRequests.h"
#include "YPPServiceRequest.h"
#include <ace/Version.h>
#include <iostream>
#include <yarp/conf/version.h>
#include <yarp/os/all.h>

using namespace YarpPlusPlus;
using namespace YarpPlusPlusTest;
using std::cout;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief Set to @c true to use an in-memory database and @c false to use a disk-based database. */
#define TEST_INMEMORY true

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
    Endpoint * stuff = NULL;
    
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
    return stuff;
} // doCreateEndpointForTest

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
    int        result;
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
            result = 1;
        }
        delete stuff;
    }
    else
    {
        result = 1;
    }
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
    int        result;
    Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;
        
        if (stuff->open() && stuff->setReporter(reporter, true))
        {
            OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
            // Now we try to connect!
            yarp::os::Port        outPort;
            yarp::os::ConstString aName(Endpoint::GetRandomPortName());
            
            OD_SYSLOG_S1("opening ", aName.c_str());//####
            if (outPort.open(aName))
            {
                OD_SYSLOG("(outPort.open(aName))");//####
                outPort.getReport(reporter);
                if (outPort.addOutput(stuff->getName()))
                {
                    OD_SYSLOG("(outPort.addOutput(stuff->getName()))");//####
                    result = 0;
                }
                else
                {
                    OD_SYSLOG("! (outPort.addOutput(stuff->getName()))");//####
                    result = 1;
                }
                outPort.close();
            }
            else
            {
                OD_SYSLOG("! (outPort.open(aName))");//####
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        delete stuff;
    }
    else
    {
        result = 1;
    }
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
    int        result;
    Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;
        Test03Handler          handler;
        
        if (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, true))
        {
            OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
            // Now we try to connect!
            yarp::os::Port        outPort;
            yarp::os::ConstString aName(Endpoint::GetRandomPortName());
            
            OD_SYSLOG_S1("opening ", aName.c_str());//####
            if (outPort.open(aName))
            {
                OD_SYSLOG("(outPort.open(aName))");//####
                outPort.getReport(reporter);
                if (outPort.addOutput(stuff->getName()))
                {
                    OD_SYSLOG("(outPort.addOutput(stuff->getName()))");//####
                    yarp::os::Bottle message;
                    
                    message.addString(aName);
                    message.addString("howdi");
                    if (outPort.write(message))
                    {
                        OD_SYSLOG("(outPort.write(message))");//####
                        result = 0;
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort.write(message))");//####
                        result = 1;
                    }
                }
                else
                {
                    OD_SYSLOG("! (outPort.addOutput(stuff->getName()))");//####
                    result = 1;
                }
                outPort.close();
            }
            else
            {
                OD_SYSLOG("! (outPort.open(aName))");//####
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        delete stuff;
    }
    else
    {
        result = 1;
    }
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
    int        result;
    Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;
        Test04Handler          handler;
        
        if (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, true))
        {
            OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
            // Now we try to connect!
            yarp::os::Port        outPort;
            yarp::os::ConstString aName(Endpoint::GetRandomPortName());
            
            OD_SYSLOG_S1("opening ", aName.c_str());//####
            if (outPort.open(aName))
            {
                OD_SYSLOG("(outPort.open(aName))");//####
                outPort.getReport(reporter);
                if (outPort.addOutput(stuff->getName()))
                {
                    OD_SYSLOG("(outPort.addOutput(stuff->getName()))");//####
                    yarp::os::Bottle message;
                    yarp::os::Bottle response;
                    
                    message.addString(aName);
                    message.addString("howdi");
                    if (outPort.write(message, response))
                    {
                        OD_SYSLOG("(outPort.write(message, response))");//####
                        OD_SYSLOG_S1("got ", response.toString().c_str());//####
                        result = 0;
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort.write(message, response))");//####
                        result = 1;
                    }
                }
                else
                {
                    OD_SYSLOG("! (outPort.addOutput(stuff->getName()))");//####
                    result = 1;
                }
                outPort.close();
            }
            else
            {
                OD_SYSLOG("! (outPort.open(aName))");//####
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        delete stuff;
    }
    else
    {
        result = 1;
    }
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
    int        result;
    Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;
        Test05HandlerCreator   handlerCreator;
        
        if (stuff->setInputHandlerCreator(handlerCreator) && stuff->open() && stuff->setReporter(reporter, true))
        {
            OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
            // Now we try to connect!
            yarp::os::Port        outPort;
            yarp::os::ConstString aName(Endpoint::GetRandomPortName());
            
            OD_SYSLOG_S1("opening ", aName.c_str());//####
            if (outPort.open(aName))
            {
                OD_SYSLOG("(outPort.open(aName))");//####
                outPort.getReport(reporter);
                if (outPort.addOutput(stuff->getName()))
                {
                    OD_SYSLOG("(outPort.addOutput(stuff->getName()))");//####
                    yarp::os::Bottle message;
                    yarp::os::Bottle response;
                    
                    message.addString(aName);
                    message.addString("howdi");
                    if (outPort.write(message, response))
                    {
                        OD_SYSLOG("(outPort.write(message, response))");//####
                        OD_SYSLOG_S1("got ", response.toString().c_str());//####
                        result = 0;
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort.write(message, response))");//####
                        result = 1;
                    }
                }
                else
                {
                    OD_SYSLOG("! (outPort.addOutput(stuff->getName()))");//####
                    result = 1;
                }
                outPort.close();
            }
            else
            {
                OD_SYSLOG("! (outPort.open(aName))");//####
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        delete stuff;
    }
    else
    {
        result = 1;
    }
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
    int result;
    
    if (0 == argc)
    {
        result = 1;
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
    int              result;
    yarp::os::Bottle parameters;
    
    for (int ii = 0; ii < argc; ++ii)
    {
        parameters.addString(argv[ii]);
    }
    ServiceResponse * stuff = new ServiceResponse(parameters);
    
    delete stuff;
    result = 0;
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
    int        result;
    Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;
        Test08Handler          handler;
        
        if (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, true))
        {
            OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
            yarp::os::Bottle parameters("some to send");
            ServiceRequest   request(YPP_ECHO_REQUEST, parameters);
            ServiceResponse  response;
            
            if (request.send(*stuff, NULL, &response))
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
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        delete stuff;
    }
    else
    {
        result = 1;
    }
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
    int             result;
    Test09Service * stuff = new Test09Service(argc, argv);
    
    if (stuff && stuff->start())
    {
        yarp::os::Bottle parameters("some to send");
        ServiceRequest   request(YPP_ECHO_REQUEST, parameters);
        ServiceResponse  response;
        
        if (request.send(stuff->getEndpoint(), NULL, &response))
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
            result = 1;
        }
        stuff->stop();
        delete stuff;
    }
    else
    {
        result = 1;
    }
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
    int             result;
    Test10Service * stuff = new Test10Service(argc, argv);
    
    if (stuff && stuff->start())
    {
        yarp::os::Bottle parameters("some to send");
        ServiceRequest   request(YPP_ECHO_REQUEST, parameters);
        ServiceResponse  response;
        
        if (request.send(stuff->getEndpoint(), NULL, &response))
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
            result = 1;
        }
        stuff->stop();
        delete stuff;
    }
    else
    {
        result = 1;
    }
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
    int             result;
    Test11Service * stuff = new Test11Service(argc, argv);
    
    if (stuff && stuff->start())
    {
        yarp::os::Bottle parameters("some to send");
        ServiceRequest   request(YPP_ECHO_REQUEST, parameters);
        ServiceResponse  response;
        
        if (request.send(stuff->getEndpoint(), NULL, &response))
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
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        stuff->stop();
        delete stuff;
    }
    else
    {
        result = 1;
    }
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
    bool result;
    
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
        result = false;
    }
    return result;
} // checkList12Response

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase12(const int argc,
                    char **   argv) // send 'list' request
{
    int             result;
    Test12Service * stuff = new Test12Service(argc, argv);
    
    if (stuff && stuff->start())
    {
        ServiceRequest  request(YPP_LIST_REQUEST);
        ServiceResponse response;
        
        if (request.send(stuff->getEndpoint(), NULL, &response))
        {
            OD_SYSLOG_LL1("response size = ", response.count());//####
            for (int ii = 0; ii < response.count(); ++ii)
            {
                OD_SYSLOG_S1("response value = ", response.element(ii).toString().c_str());//####
            }
            result = (checkList12Response(response) ? 0 : 1);
        }
        else
        {
            result = 1;
        }
        stuff->stop();
        delete stuff;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase12

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase13(const int argc,
                    char **   argv) // send 'register' request
{
    int               result;
    RegistryService * registry = NULL;
    
    if (0 <= argc)
    {
        switch (argc)
        {
                // Argument order for tests = endpoint name [, IP address / name [, port]]
            case 0:
                registry = new RegistryService(TEST_INMEMORY);
                break;
                
            case 1:
                registry = new RegistryService(TEST_INMEMORY, *argv);
                break;
                
            case 2:
                registry = new RegistryService(TEST_INMEMORY, *argv, argv[1]);
                break;
                
            default:
                break;
                
        }
    }
    if (registry && registry->start())
    {
        result = (registry->isActive() ? 0 : 1);
        registry->stop();
        delete registry;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase13

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase14(const int argc,
                    char **   argv) // send 'register' request
{
    const char *      secondServicePort;
    int               result;
    RegistryService * registry = NULL;
    
    if (0 <= argc)
    {
        switch (argc)
        {
            // Argument order for tests = endpoint name [, IP address / name [, port]]
            case 0:
                registry = new RegistryService(TEST_INMEMORY);
                secondServicePort = "/service/test14_1";
                break;
                
            case 1:
                registry = new RegistryService(TEST_INMEMORY, *argv);
                secondServicePort = "/service/test14_2";
                break;
                
            case 2:
                registry = new RegistryService(TEST_INMEMORY, *argv, argv[1]);
                secondServicePort = "/service/test14_3";
                break;
                
            default:
                break;
                
        }
    }
    if (registry && registry->start())
    {
        if (registry->isActive())
        {
            // Now we start up another service (Test14Service) and register it
            Test14Service * stuff = new Test14Service(1, const_cast<char **>(&secondServicePort));
            
            if (stuff && stuff->start())
            {
                yarp::os::ConstString portName(stuff->getEndpoint().getName());

                if (YarpPlusPlus::RegisterLocalService(portName))
                {
                    result = 0;
                }
                else
                {
                    result = 1;
                }
                stuff->stop();
                delete stuff;
            }
            else
            {
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        registry->stop();
        delete registry;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase14

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase15(const int argc,
                    char **   argv) // send 'register' request
{
    const char *      secondServicePort;
    int               result;
    RegistryService * registry = NULL;
    
    if (0 <= argc)
    {
        switch (argc)
        {
            // Argument order for tests = endpoint name [, IP address / name [, port]]
            case 0:
                registry = new RegistryService(TEST_INMEMORY);
                secondServicePort = "/service/test15_1";
                break;
                
            case 1:
                registry = new RegistryService(TEST_INMEMORY, *argv);
                secondServicePort = "/service/test15_2";
                break;
                
            case 2:
                registry = new RegistryService(TEST_INMEMORY, *argv, argv[1]);
                secondServicePort = "/service/test15_3";
                break;
                
            default:
                break;
                
        }
    }
    if (registry && registry->start())
    {
        if (registry->isActive())
        {
            // Now we start up another service (Test15Service) and register it
            Test15Service * stuff = new Test15Service(1, const_cast<char **>(&secondServicePort));
            
            if (stuff && stuff->start())
            {
                yarp::os::ConstString portName(stuff->getEndpoint().getName());
                
                if (YarpPlusPlus::RegisterLocalService(portName))
                {
                    if (YarpPlusPlus::UnregisterLocalService(portName))
                    {
                        result = 0;
                    }
                    else
                    {
                        result = 1;
                    }
                }
                else
                {
                    result = 1;
                }
                stuff->stop();
                delete stuff;
            }
            else
            {
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        registry->stop();
        delete registry;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase15

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase16(const int argc,
                    char **   argv) // send 'register' request
{
    int result;

    if (1 < argc)
    {
        const char *      secondServicePort = "/service/test16";
        RegistryService * registry = new RegistryService(TEST_INMEMORY);
        
        if (registry && registry->start())
        {
            if (registry->isActive())
            {
                // Now we start up another service (Test16Service) and register it
                Test16Service * stuff = new Test16Service(1, const_cast<char **>(&secondServicePort));
                
                if (stuff && stuff->start())
                {
                    yarp::os::ConstString portName(stuff->getEndpoint().getName());
                    
                    if (YarpPlusPlus::RegisterLocalService(portName))
                    {
                        // Search for the service that we just registered.
                        yarp::os::Bottle matches(YarpPlusPlus::FindMatchingServices(*argv));
                        yarp::os::Bottle expected(argv[1]);
                        
                        OD_SYSLOG_S3("criteria <- ", *argv, "expected <- ", expected.toString().c_str(),//####
                                     "matches <- ", matches.toString().c_str());//####
                        if ((expected.size() == matches.size()) &&
                            (BaseClient::kExpectedResponseSize == matches.size()))
                        {
                            bool            wasASuccess = false;
                            yarp::os::Value matchesFirst(matches.get(0));
                            yarp::os::Value expectedFirst(expected.get(0));

                            if (expectedFirst.isString())
                            {
                                yarp::os::ConstString matchesFirstAsString(matchesFirst.toString());
                                yarp::os::ConstString expectedFirstAsString(expectedFirst.toString());
                                
                                if (matchesFirstAsString == expectedFirstAsString)
                                {
                                    result = 0;
                                    if (! strcmp(YPP_OK_RESPONSE, matchesFirstAsString.c_str()))
                                    {
                                        wasASuccess = true;
                                    }
                                }
                                else
                                {
                                    result = 1;
                                }
                            }
                            else
                            {
                                result = 1;
                            }
                            if ((! result) && wasASuccess)
                            {
                                yarp::os::Value matchesSecond(matches.get(1));
                                yarp::os::Value expectedSecond(expected.get(1));
                                
                                if (expectedSecond.isList())
                                {
                                    yarp::os::Bottle * matchesSecondAsList = matchesSecond.asList();
                                    yarp::os::Bottle * expectedSecondAsList = expectedSecond.asList();
                                    int                matchesSecondCount = matchesSecondAsList->size();
                                    int                expectedSecondCount = expectedSecondAsList->size();
                                    
                                    OD_SYSLOG_LL2("matchesSecondCount <- ", matchesSecondCount,//####
                                                  "expectedSecondCount <- ", expectedSecondCount);//####
                                    if (matchesSecondCount == expectedSecondCount)
                                    {
                                        // Since the lists are the same length, we can just look for the expected values
                                        // in the matched values.
                                        for (int ii = 0; ii < expectedSecondCount; ++ii)
                                        {
                                            bool                  didFind = false;
                                            yarp::os::Value       expectedSecondValue(expectedSecondAsList->get(ii));
                                            yarp::os::ConstString expectedString(expectedSecondValue.toString());
                                            
                                            for (int jj = 0; jj < expectedSecondCount; ++jj)
                                            {
                                                yarp::os::Value       matchesSecondValue(matchesSecondAsList->get(jj));
                                                yarp::os::ConstString matchesString(matchesSecondValue.toString());
                                                
                                                if (expectedString == matchesString)
                                                {
                                                    didFind = true;
                                                    break;
                                                }
                                                
                                            }
                                            if (! didFind)
                                            {
                                                result = 1;
                                                break;
                                            }
                                            
                                        }
                                    }
                                    else
                                    {
                                        result = 1;
                                    }
                                }
                                else
                                {
                                    result = 1;
                                }
                            }
                        }
                        else
                        {
                            result = 1;
                        }
                        if (! YarpPlusPlus::UnregisterLocalService(portName))
                        {
                            result = 1;
                        }
                    }
                    else
                    {
                        result = 1;
                    }
                    stuff->stop();
                    delete stuff;
                }
                else
                {
                    result = 1;
                }
            }
            else
            {
                result = 1;
            }
            registry->stop();
            delete registry;
        }
        else
        {
            result = 1;
        }
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase16

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
                   kODSyslogOptionEnableThreadSupport);//####
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("YARP Version = ", YARP_VERSION_STRING, "YARP++ Version = ", YPP_VERSION, "ACE Version = ",//####
                 ACE_VERSION);//####
    yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
    int               result;
    
    if (0 < --argc)
    {
        int selector = atoi(argv[1]);
        
        OD_SYSLOG_LL1("selector <- ", selector);//####
        switch (selector)
        {
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
            
            case 13:
                result = doCase13(argc - 1, argv + 2);
                break;
                
            case 14:
                result = doCase14(argc - 1, argv + 2);
                break;
                
            case 15:
                result = doCase15(argc - 1, argv + 2);
                break;

            case 16:
                result = doCase16(argc - 1, argv + 2);
                break;
                
            default:
                result = 1;
                break;
                
        }
    }
    else
    {
        result = 1;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // main
