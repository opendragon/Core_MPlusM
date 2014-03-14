//--------------------------------------------------------------------------------------
//
//  File:       YPPRegistryTest.cpp
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
//  Created:    2014-03-14
//
//--------------------------------------------------------------------------------------

//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "RegistryTests/YPPTTest14Service.h"
#include "RegistryTests/YPPTTest15Service.h"
#include "RegistryTests/YPPTTest16Service.h"
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
