//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRunningSumClientMain.cpp
//
//  Project:    m+m
//
//  Contains:   The main application for the Running Sum client.
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

#include "m+mRunningSumClient.hpp"

#include <m+m/m+mUtilities.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the Running Sum client. */
/*! @dir RunningSumClient
 @brief The set of files that implement the Running Sum client. */
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

/*! @brief Display the available commands. */
static void
displayCommands(void)
{
    ODL_ENTER(); //####
    cout << "Commands:" << endl;
    cout << "  ? - display this list" << endl;
    cout << "  + - read a number from the terminal and update the running sum" << endl;
    cout << "  q - quit the application" << endl;
    cout << "  r - reset the running sum" << endl;
    cout << "  s - start calculating the running sum" << endl;
    ODL_EXIT(); //####
} // displayCommands

/*! @brief Set up the environment and perform the operation. */
#if defined(MpM_ReportOnConnections)
static void
setUpAndGo(ChannelStatusReporter * reporter)
#else // ! defined(MpM_ReportOnConnections)
static void
setUpAndGo(void)
#endif // ! defined(MpM_ReportOnConnections)
{
    ODL_ENTER(); //####
#if defined(MpM_ReportOnConnections)
    ODL_P1("reporter = ", reporter); //####
#endif // defined(MpM_ReportOnConnections)
    RunningSumClient * aClient = new RunningSumClient;

    if (aClient)
    {
        StartRunning();
        SetSignalHandlers(SignalRunningStop);
        if (aClient->findService("Name: RunningSum"))
        {
#if defined(MpM_ReportOnConnections)
            aClient->setReporter(*reporter, true);
#endif // defined(MpM_ReportOnConnections)
            if (aClient->connectToService())
            {
                for ( ; IsRunning(); )
                {
                    char   inChar;
                    double newSum;
                    double value;

                    cout << "Operation: [? + q r s]? ";
                    cout.flush();
                    cin >> inChar;
                    switch (inChar)
                    {
                        case '?' :
                            // Help
                            displayCommands();
                            break;

                        case '+' :
                            cout << "add: ";
                            cout.flush();
                            cin >> value;
                            cout << "adding " << value << endl;
                            if (aClient->addToSum(value, newSum))
                            {
                                cout << "running sum = " << newSum << endl;
                            }
                            else
                            {
                                ODL_LOG("! (aClient->addToSum(value, newSum))"); //####
                                MpM_FAIL_("Problem adding to the sum.");
                            }
                            break;

                        case 'q' :
                        case 'Q' :
                            cout << "Exiting" << endl;
                            if (! aClient->stopSum())
                            {
                                ODL_LOG("(! aClient->stopSum())"); //####
                                MpM_FAIL_("Problem stopping the sum.");
                            }
                            StopRunning();
                            break;

                        case 'r' :
                        case 'R' :
                            cout << "Resetting" << endl;
                            if (! aClient->resetSum())
                            {
                                ODL_LOG("(! aClient->resetSum())"); //####
                                MpM_FAIL_("Problem resetting the sum.");
                            }
                            break;

                        case 's' :
                        case 'S' :
                            cout << "Starting" << endl;
                            if (! aClient->startSum())
                            {
                                ODL_LOG("(! aClient->startSum())"); //####
                                MpM_FAIL_("Problem starting the sum.");
                            }
                            break;

                        default :
                            cout << "Unrecognized request '" << inChar << "'." << endl;
                            break;

                    }
                }
                if (! aClient->disconnectFromService())
                {
                    ODL_LOG("(! aClient->disconnectFromService())"); //####
                    MpM_FAIL_(MSG_COULD_NOT_DISCONNECT_FROM_SERVICE);
                }
            }
            else
            {
                ODL_LOG("! (aClient->connectToService())"); //####
                MpM_FAIL_(MSG_COULD_NOT_CONNECT_TO_SERVICE);
            }
        }
        else
        {
            ODL_LOG("! (aClient->findService(\"Name: RunningSum\")"); //####
            MpM_FAIL_(MSG_COULD_NOT_FIND_SERVICE);
        }
        delete aClient;
    }
    else
    {
        ODL_LOG("! (aClient)"); //####
    }
    ODL_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

#if (! defined(MAC_OR_LINUX_))
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! defined(MAC_OR_LINUX_)
/*! @brief The entry point for communicating with the Running Sum service.

 Commands from standard input will be interpreted and will result in requests being sent to the
 service.
 Commands are not case-sensitive. The commands are:

 @c ? Display the list of commands.

 @c + Read a number from standard input and send it to the service.

 @c r Ask the service to reset its running sum.

 @c s Ask the service to start calculating the running sum.

 @c x Ask the service to stop calculating the running sum and exit from the program.

 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the application.
 @returns @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
#if defined(MAC_OR_LINUX_)
# pragma unused(argc)
#endif // defined(MAC_OR_LINUX_)
    YarpString progName(*argv);

    ODL_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport | //####
             kODLoggingOptionWriteToStderr); //####
    ODL_ENTER(); //####
#if defined(MAC_OR_LINUX_)
    SetUpLogger(progName);
#endif // defined(MAC_OR_LINUX_)
    try
    {
        Utilities::DescriptorVector argumentList;
        OutputFlavour               flavour;

        if (Utilities::ProcessStandardClientOptions(argc, argv, argumentList,
                                                    "The client for the Running Sum service",
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
                        ODL_LOG("! (Utilities::CheckForRegistryService())"); //####
                        MpM_FAIL_(MSG_REGISTRY_NOT_RUNNING);
                    }
                }
                else
                {
                    ODL_LOG("! (Utilities::CheckForValidNetwork())"); //####
                    MpM_FAIL_(MSG_YARP_NOT_RUNNING);
                }
                Utilities::ShutDownGlobalStatusReporter();
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    ODL_EXIT_L(0); //####
    return 0;
} // main
#if (! defined(MAC_OR_LINUX_))
# pragma warning(pop)
#endif // ! defined(MAC_OR_LINUX_)
