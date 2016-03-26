//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRegistryTest.cpp
//
//  Project:    m+m
//
//  Contains:   The test driver for the unit tests of the Service Registry ervice.
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
//  Created:    2014-03-14
//
//--------------------------------------------------------------------------------------------------

#include "m+mRegistryService.h"
#include "RegistryTests/m+mTest14Service.h"
#include "RegistryTests/m+mTest15Service.h"
#include "RegistryTests/m+mTest16Service.h"

#include <m+m/m+mBaseClient.h>
#include <m+m/m+mBaseRequestHandler.h>
#include <m+m/m+mEndpoint.h>
#include <m+m/m+mRequests.h>
#include <m+m/m+mServiceRequest.h>
#include <m+m/m+mUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The test driver for the unit tests of the %Registry Service. */

/*! @dir /RegistryTests
 @brief The set of files that provide test cases for the %Registry Service. */
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

/*! @brief Set to @c true to use an in-memory database and @c false to use a disk-based database. */
#define TEST_INMEMORY_ true

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestCreateRegistryService(const char * launchPath,
                            const int    argc,
                            char * *     argv) // create 'register' service
{
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        Registry::RegistryService * registry = NULL;
        
        if (0 <= argc)
        {
            switch (argc)
            {
                    // Argument order for tests = [IP address / name [, port]]
                case 0 :
                    registry = new Registry::RegistryService(launchPath, argc, argv,
                                                             TEST_INMEMORY_);
                    break;
                    
                case 1 :
                    registry = new Registry::RegistryService(launchPath, argc, argv, TEST_INMEMORY_,
                                                             *argv);
                    break;
                    
                default :
                    break;
                    
            }
        }
        if (registry)
        {
            if (registry->startService())
            {
                result = (registry->isActive() ? 0 : 1);
                registry->stopService();
            }
            else
            {
                ODL_LOG("! (registry->startService())"); //####
            }
            delete registry;
        }
        else
        {
            ODL_LOG("! (registry)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestCreateRegistryService

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestRequestRegistryService(const char * launchPath,
                             const int    argc,
                             char * *     argv) // send 'register' request
{
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    int result = false;
    
    try
    {
        const char *                secondServiceChannel;
        Registry::RegistryService * registry = NULL;
        
        if (0 <= argc)
        {
            switch (argc)
            {
                    // Argument order for tests = [IP address / name [, port]]
                case 0 :
                    registry = new Registry::RegistryService(launchPath, argc, argv,
                                                             TEST_INMEMORY_);
                    secondServiceChannel = BUILD_NAME_(MpM_SERVICE_BASE_NAME_, 
                                                       BUILD_NAME_("test",
                                                                   "requestregisterservice_1"));
                    break;
                    
                case 1 :
                    registry = new Registry::RegistryService(launchPath, argc, argv, TEST_INMEMORY_,
                                                             *argv);
                    secondServiceChannel = BUILD_NAME_(MpM_SERVICE_BASE_NAME_, 
                                                       BUILD_NAME_("test",
                                                                   "requestregisterservice_2"));
                    break;
                    
                default :
                    break;
                    
            }
        }
        if (registry)
        {
            if (registry->startService())
            {
                if (registry->isActive())
                {
                    // Now we start up another service (Test14Service) and register it
                    Test14Service * aService = new Test14Service(launchPath, 1,
                                                      const_cast<char * *>(&secondServiceChannel));
                    
                    if (aService)
                    {
                        if (aService->startService())
                        {
                            YarpString channelName(aService->getEndpoint().getName());
                            
                            if (RegisterLocalService(channelName, *aService))
                            {
                                result = 0;
                            }
                            else
                            {
                                ODL_LOG("! (RegisterLocalService(channelName, *aService))"); //####
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
                else
                {
                    ODL_LOG("! (registry->isActive())"); //####
                }
                registry->stopService();
            }
            else
            {
                ODL_LOG("! (registry->startService())"); //####
            }
            delete registry;
        }
        else
        {
            ODL_LOG("! (registry)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestRequestRegistryService

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestRequestUnregisterService(const char * launchPath,
                               const int    argc,
                               char * *     argv) // send 'register' request
{
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        const char *                secondServiceChannel;
        Registry::RegistryService * registry = NULL;
        
        if (0 <= argc)
        {
            switch (argc)
            {
                    // Argument order for tests = [IP address / name [, port]]
                case 0 :
                    registry = new Registry::RegistryService(launchPath, argc, argv,
                                                             TEST_INMEMORY_);
                    secondServiceChannel = BUILD_NAME_(MpM_SERVICE_BASE_NAME_, 
                                                       BUILD_NAME_("test",
                                                                   "requestunregisterservice_1"));
                    break;
                    
                case 1 :
                    registry = new Registry::RegistryService(launchPath, argc, argv, TEST_INMEMORY_,
                                                             *argv);
                    secondServiceChannel = BUILD_NAME_(MpM_SERVICE_BASE_NAME_, 
                                                       BUILD_NAME_("test",
                                                                   "requestunregisterservice_2"));
                    break;
                    
                default :
                    break;
                    
            }
        }
        if (registry)
        {
            if (registry->startService())
            {
                if (registry->isActive())
                {
                    // Now we start up another service (Test15Service) and register it
                    Test15Service * aService = new Test15Service(launchPath, 1,
                                                      const_cast<char * *>(&secondServiceChannel));
                    
                    if (aService)
                    {
                        if (aService->startService())
                        {
                            YarpString channelName(aService->getEndpoint().getName());
                            
                            if (RegisterLocalService(channelName, *aService))
                            {
                                if (UnregisterLocalService(channelName, *aService))
                                {
                                    result = 0;
                                }
                                else
                                {
                                    ODL_LOG("! (UnregisterLocalService(channelName, " //####
                                           "*aService))"); //####
                                }
                            }
                            else
                            {
                                ODL_LOG("! (RegisterLocalService(channelName, *aService))"); //####
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
                else
                {
                    ODL_LOG("! (registry->isActive())"); //####
                }
                registry->stopService();
            }
            else
            {
                ODL_LOG("! (registry->startService())"); //####
            }
            delete registry;
        }
        else
        {
            ODL_LOG("! (registry)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestRequestUnregisterService

/*! @brief Perform a test case.
 @param launchPath The command-line name used to launch the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestRequestSearchService(const char * launchPath,
                           const int    argc,
                           char * *     argv) // send 'match' request
{
    ODL_ENTER(); //####
    ODL_S1("launchPath = ", launchPath); //####
    int result = 1;
    
    try
    {
        if (1 < argc)
        {
            bool                        getNamesFlag = ('0' != **argv);
            const char *                secondServiceChannel = BUILD_NAME_(MpM_SERVICE_BASE_NAME_, 
                                                                           BUILD_NAME_("test",
                                                                           "requestsearchservice"));
            Registry::RegistryService * registry = new Registry::RegistryService(launchPath, argc,
                                                                                 argv,
                                                                                 TEST_INMEMORY_);
            
            ODL_B1("getNamesFlag <- ", getNamesFlag); //####
            if (registry)
            {
                ODL_LOG("(registry)"); //####
                if (registry->startService())
                {
                    ODL_LOG("(registry->startService())"); //####
                    if (registry->isActive())
                    {
                        // Now we start up another service (Test16Service) and register it
                        ODL_LOG("(registry->isActive())"); //####
                        Test16Service * aService = new Test16Service(launchPath, 1,
                                                      const_cast<char * *>(&secondServiceChannel));
                        
                        if (aService)
                        {
                            ODL_LOG("(aService)"); //####
                            if (aService->startService())
                            {
                                ODL_LOG("(aService->start())"); //####
                                YarpString channelName(aService->getEndpoint().getName());
                                
                                if (RegisterLocalService(channelName, *aService))
                                {
                                    ODL_LOG("(RegisterLocalService(channelName, *aService))"); //####
                                    // Search for the service that we just registered.
                                    yarp::os::Bottle matches = FindMatchingServices(argv[1],
                                                                                    getNamesFlag);
                                    
                                    ODL_S3("criteria <- ", argv[1], "expected <- ", //####
                                              argv[2], "matches <- ", //####
                                              matches.toString().c_str()); //####
                                    yarp::os::Bottle expected(argv[2]);
                                    
                                    if ((expected.size() == matches.size()) &&
                                        (MpM_EXPECTED_MATCH_RESPONSE_SIZE_ == matches.size()))
                                    {
                                        ODL_LOG("((expected.size() == matches.size()) && " //####
                                               "(MpM_EXPECTED_MATCH_RESPONSE_SIZE_ == " //####
                                               "matches.size()))"); //####
                                        bool            wasASuccess = false;
                                        yarp::os::Value matchesFirst(matches.get(0));
                                        yarp::os::Value expectedFirst(expected.get(0));
                                        
                                        if (expectedFirst.isString())
                                        {
                                            ODL_LOG("(expectedFirst.isString())"); //####
                                            YarpString matchesFirstAsString =
                                                                            matchesFirst.toString();
                                            YarpString expectedFirstAsString =
                                                                        expectedFirst.toString();
                                            
                                            if (matchesFirstAsString == expectedFirstAsString)
                                            {
                                                ODL_LOG("(matchesFirstAsString == " //####
                                                       "expectedFirstAsString)"); //####
                                                result = 0;
                                                if (! strcmp(MpM_OK_RESPONSE_,
                                                             matchesFirstAsString.c_str()))
                                                {
                                                    wasASuccess = true;
                                                }
                                            }
                                            else
                                            {
                                                ODL_LOG("! (matchesFirstAsString == " //####
                                                       "expectedFirstAsString)"); //####
                                            }
                                        }
                                        else
                                        {
                                            ODL_LOG("! (expectedFirst.isString())"); //####
                                        }
                                        if ((! result) && wasASuccess)
                                        {
                                            ODL_LOG("((! result) && wasASuccess)"); //####
                                            yarp::os::Value matchesSecond(matches.get(1));
                                            yarp::os::Value expectedSecond(expected.get(1));
                                            
                                            if (expectedSecond.isList())
                                            {
                                                ODL_LOG("(expectedSecond.isList())"); //####
                                                yarp::os::Bottle * matchesSecondAsList =
                                                                            matchesSecond.asList();
                                                yarp::os::Bottle * expectedSecondAsList =
                                                                            expectedSecond.asList();
                                                int                matchesSecondCount =
                                                                        matchesSecondAsList->size();
                                                int                expectedSecondCount =
                                                                    expectedSecondAsList->size();
                                                
                                                ODL_LL2("matchesSecondCount <- ", //####
                                                           matchesSecondCount, //####
                                                           "expectedSecondCount <- ", //####
                                                           expectedSecondCount); //####
                                                if (matchesSecondCount == expectedSecondCount)
                                                {
                                                    ODL_LOG("(matchesSecondCount == " //####
                                                           "expectedSecondCount)"); //####
                                                    // Since the lists are the same length, we can
                                                    // just look for the expected values in the
                                                    // matched values.
                                                    for (int ii = 0; ii < expectedSecondCount; ++ii)
                                                    {
                                                        bool            didFind = false;
                                                        yarp::os::Value expectedSecondValue =
                                                                    expectedSecondAsList->get(ii);
                                                        YarpString      expectedString =
                                                                    expectedSecondValue.toString();
                                                        
                                                        for (int jj = 0; jj < expectedSecondCount;
                                                             ++jj)
                                                        {
                                                            yarp::os::Value matchesSecondValue =
                                                                    matchesSecondAsList->get(jj);
                                                            YarpString      matchesString =
                                                                    matchesSecondValue.toString();
                                                            
                                                            if (expectedString == matchesString)
                                                            {
                                                                didFind = true;
                                                                break;
                                                            }
                                                            
                                                        }
                                                        if (! didFind)
                                                        {
                                                            ODL_LOG("(! didFind)"); //####
                                                            result = 1;
                                                            break;
                                                        }
                                                        
                                                    }
                                                }
                                                else
                                                {
                                                    ODL_LOG("! (matchesSecondCount == " //####
                                                           "expectedSecondCount)"); //####
                                                    result = 1;
                                                }
                                            }
                                            else
                                            {
                                                ODL_LOG("! (expectedSecond.isList())"); //####
                                                result = 1;
                                            }
                                        }
                                    }
                                    else
                                    {
                                        ODL_LOG("! ((expected.size() == matches.size()) && " //####
                                               "(MpM_EXPECTED_MATCH_RESPONSE_SIZE_ == " //####
                                               "matches.size()))"); //####
                                    }
                                    if (! UnregisterLocalService(channelName, *aService))
                                    {
                                        ODL_LOG("(! UnregisterLocalService(channelName, " //####
                                               "*aService))"); //####
                                    }
                                }
                                else
                                {
                                    ODL_LOG("! (RegisterLocalService(channelName, " //####
                                           "*aService))"); //####
                                }
                                aService->stopService();
                            }
                            else
                            {
                                ODL_LOG("! (aService->start())"); //####
                            }
                            delete aService;
                        }
                        else
                        {
                            ODL_LOG("! (aService)"); //####
                        }
                    }
                    else
                    {
                        ODL_LOG("! (registry->isActive())"); //####
                    }
                    registry->stopService();
                }
                else
                {
                    ODL_LOG("! (registry->start())"); //####
                }
                delete registry;
            }
            else
            {
                ODL_LOG("! (registry)"); //####
            }
        }
        else
        {
            ODL_LOG("! (1 < argc)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestRequestSearchService

/*! @brief The signal handler to catch requests to stop the service.
 @param signal The signal being handled. */
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

/*! @brief The entry point for unit tests of the %Registry Service classes.
 
 The first argument is the test number, the second argument is the name of the channel to be used
 with the test, the optional third argument is the machine to connect to and the optional fourth
 argument is the port number to be used.
 Output depends on the test being run.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the unit tests.
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
        // We normally call CheckForNameServerReporter() here, but we should only run the tests with
        // a valid YARP configuration.
        Utilities::SetUpGlobalStatusReporter();
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
                
                ODL_LL1("selector <- ", selector); //####
                if ((startPtr != endPtr) && (! *endPtr) && (0 < selector))
                {
                    SetSignalHandlers(catchSignal);
                    switch (selector)
                    {
                        case 13 :
                            result = doTestCreateRegistryService(*argv, argc - 1, argv + 2);
                            break;
                            
                        case 14 :
                            result = doTestRequestRegistryService(*argv, argc - 1, argv + 2);
                            break;
                            
                        case 15 :
                            result = doTestRequestUnregisterService(*argv, argc - 1, argv + 2);
                            break;
                            
                        case 16 :
                            result = doTestRequestSearchService(*argv, argc - 1, argv + 2);
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
    }
    yarp::os::Network::fini();
    ODL_EXIT_L(result); //####
    return result;
} // main
