//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRandomNumberClientMain.cpp
//
//  Project:    m+m
//
//  Contains:   The main application for the Random Number client.
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
//  Created:    2014-03-06
//
//--------------------------------------------------------------------------------------------------

#include "m+mRandomNumberClient.h"

#include <m+m/m+mUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the Random Number client. */

/*! @dir RandomNumberClient
 @brief The set of files that implement the Random Number client. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;
using std::cerr;
using std::cin;
using std::cout;
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

/*! @brief Set up the environment and perform the operation. */
#if defined(MpM_ReportOnConnections)
static void
setUpAndGo(ChannelStatusReporter * reporter)
#else // ! defined(MpM_ReportOnConnections)
static void
setUpAndGo(void)
#endif // ! defined(MpM_ReportOnConnections)
{
    OD_LOG_ENTER(); //####
#if defined(MpM_ReportOnConnections)
    OD_LOG_P1("reporter = ", reporter); //####
#endif // defined(MpM_ReportOnConnections)
    RandomNumberClient * aClient = new RandomNumberClient;
    
    if (aClient)
    {
        StartRunning();
        SetSignalHandlers(SignalRunningStop);
        if (aClient->findService("keyword: random"))
        {
#if defined(MpM_ReportOnConnections)
            aClient->setReporter(*reporter, true);
#endif // defined(MpM_ReportOnConnections)
            if (aClient->connectToService())
            {
                for ( ; IsRunning(); )
                {
                    int count;
                    
                    cout << "How many random numbers? ";
                    cout.flush();
                    cin >> count;
                    if (0 >= count)
                    {
                        break;
                    }
                    
                    if (1 == count)
                    {
                        double result;
                        
                        if (aClient->getOneRandomNumber(result))
                        {
                            cout << "result = " << result << endl;
                        }
                        else
                        {
                            OD_LOG("! (aClient->getOneRandomNumber(result))"); //####
#if MAC_OR_LINUX_
                            GetLogger().fail("Problem getting random number from service.");
#endif // MAC_OR_LINUX_
                        }
                    }
                    else
                    {
                        DoubleVector results;
                        
                        if (aClient->getRandomNumbers(count, results))
                        {
                            cout << "result = (";
                            if (0 < results.size())
                            {
                                for (DoubleVector::const_iterator it = results.begin();
                                     results.end() != it; ++it)
                                {
                                    cout << " " << *it;
                                }
                                cout << " )" << endl;
                            }
                        }
                        else
                        {
                            OD_LOG("! (aClient->getRandomNumbers(count, results))"); //####
#if MAC_OR_LINUX_
                            GetLogger().fail("Problem getting random numbers from service.");
#endif // MAC_OR_LINUX_
                        }
                    }
                }
                if (! aClient->disconnectFromService())
                {
                    OD_LOG("(! aClient->disconnectFromService())"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("Problem disconnecting from the service.");
#endif // MAC_OR_LINUX_
                }
            }
            else
            {
                OD_LOG("! (aClient->connectToService())"); //####
#if MAC_OR_LINUX_
                GetLogger().fail("Could not connect to the required service.");
#else // ! MAC_OR_LINUX_
                cerr << "Could not connect to the required service." << endl;
#endif // ! MAC_OR_LINUX_
            }
        }
        else
        {
            OD_LOG("! (aClient->findService(\"keyword: random\"))"); //####
#if MAC_OR_LINUX_
            GetLogger().fail("Could not find the required service.");
#else // ! MAC_OR_LINUX_
            cerr << "Could not find the required service." << endl;
#endif // ! MAC_OR_LINUX_
        }
        delete aClient;
    }
    else
    {
        OD_LOG("! (aClient)"); //####
    }
    OD_LOG_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief The entry point for communicating with the Random Number service.
 
 Integers read from standard input will be sent to the service as the number of random numbers to
 generate. Entering a zero will exit the program.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the application.
 @returns @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
#if MAC_OR_LINUX_
# pragma unused(argc)
#endif // MAC_OR_LINUX_
    YarpString progName(*argv);

    OD_LOG_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
                kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport | //####
                kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(progName);
#endif // MAC_OR_LINUX_
    try
    {
        Utilities::DescriptorVector argumentList;
        OutputFlavour               flavour;
        
        if (Utilities::ProcessStandardClientOptions(argc, argv, argumentList,
                                                    "The client for the Random Number service",
                                                    2014, STANDARD_COPYRIGHT_NAME_, flavour, true))
        {
            if (CanReadFromStandardInput())
            {
                Utilities::SetUpGlobalStatusReporter();
                Utilities::CheckForNameServerReporter();
                if (Utilities::CheckForValidNetwork())
                {
#if defined(MpM_ReportOnConnections)
                    ChannelStatusReporter * reporter = Utilities::GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)
                    yarp::os::Network       yarp; // This is necessary to establish any connections
                                                  // to the YARP infrastructure
                    
                    Initialize(progName);
                    if (Utilities::CheckForRegistryService())
                    {
#if defined(MpM_ReportOnConnections)
                        setUpAndGo(reporter);
#else // ! defined(MpM_ReportOnConnections)
                        setUpAndGo();
#endif // ! defined(MpM_ReportOnConnections)
                    }
                    else
                    {
                        OD_LOG("! (Utilities::CheckForRegistryService())"); //####
#if MAC_OR_LINUX_
                        GetLogger().fail("Registry Service not running.");
#else // ! MAC_OR_LINUX_
                        cerr << "Registry Service not running." << endl;
#endif // ! MAC_OR_LINUX_
                    }
                }
                else
                {
                    OD_LOG("! (Utilities::CheckForValidNetwork())"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("YARP network not running.");
#else // ! MAC_OR_LINUX_
                    cerr << "YARP network not running." << endl;
#endif // ! MAC_OR_LINUX_
                }
                Utilities::ShutDownGlobalStatusReporter();
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_
