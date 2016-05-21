//--------------------------------------------------------------------------------------------------
//
//  File:       MovementDbClientMain.cpp
//
//  Project:    m+m
//
//  Contains:   The main application for the client of the movement database service.
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
//  Created:    2014-09-02
//
//--------------------------------------------------------------------------------------------------

#include "m+mMovementDbClient.hpp"

#include <m+m/m+mUtilities.hpp>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the client of the movement database service. */

/*! @dir MovementDbClient
 @brief The set of files that implement the movement database client. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::MovementDb;
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
    cout << "  + - request a file path adn add it to the database" << endl;
    cout << "  d - set the data track" << endl;
    cout << "  e - set the e-mail address" << endl;
    cout << "  q - quit the application" << endl;
    ODL_EXIT(); //####
} // displayCommands

#if 0
/*! @brief Write out a time value in a human-friendly form.
 @param[in] measurement The time value to write out. */
static void
reportTimeInReasonableUnits(const double measurement)
{
    double       newValue;
    const char * tag;

    if (measurement < 1e-6)
    {
        // Less than a microsecond
        newValue = (measurement * 1e6);
        tag = " microseconds";
    }
    else if (measurement < 1e-3)
    {
        // Less than a millisecond
        newValue = (measurement * 1e3);
        tag = " milliseconds";
    }
    else if (measurement < 60.0)
    {
        // Less than a minute
        newValue = measurement;
        tag = " seconds";
    }
    else if (measurement < (60.0 * 60.0))
    {
        // Less than an hour
        newValue = (measurement / 60.0);
        tag = " minutes";
    }
    else if (measurement < (24.0 * 60.0 * 60.0))
    {
        // Less than a day
        newValue = (measurement / (60.0 * 60.0));
        tag = " hours";
    }
    else
    {
        // More than a day
        newValue = (measurement / (24.0 * 60.0 * 60.0));
        tag = " days";
    }
    cout << newValue << tag;
} // reportTimeInReasonableUnits
#endif // 0

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
    MovementDbClient * aClient = new MovementDbClient;

    if (aClient)
    {
#if defined(MpM_ReportOnConnections)
        aClient->setReporter(reporter, true);
#endif // defined(MpM_ReportOnConnections)
        StartRunning();
        SetSignalHandlers(SignalRunningStop);
        if (aClient->findService("name:MovementDb"))
        {
            if (aClient->connectToService())
            {
                for ( ; IsRunning(); )
                {
                    char        inChar;
                    std::string inputLine;
                    YarpString  inputString;

                    cout << "Operation: [? + d e q]? ";
                    cout.flush();
                    if (getline(cin, inputLine))
                    {
                        inChar = inputLine[0];
                        switch (inChar)
                        {
                            case '+' :
                                cout << "add file: ";
                                cout.flush();
                                if (getline(cin, inputLine))
                                {
                                    inputString = inputLine.c_str();
                                    if (aClient->addFileToDb(inputString))
                                    {
                                        cout << "File added to database." << endl;
                                    }
                                    else
                                    {
                                        ODL_LOG("! (aClient->addFileToDb(inputString))"); //####
                                        MpM_FAIL_("Problem adding file to database.");
                                        cout << "File not added to database." << endl;
                                    }
                                }
                                else
                                {
                                    cout << "Empty file path provided." << endl;
                                }
                                break;

                            case 'd' :
                            case 'D' :
                                cout << "set data track: ";
                                cout.flush();
                                getline(cin, inputLine);
                                inputString = inputLine.c_str();
                                if (aClient->setDataTrackForDb(inputString))
                                {
                                    cout << "Data track set for database." << endl;
                                }
                                else
                                {
                                    ODL_LOG("! (aClient->setDataTrackForDb(inputString))"); //####
                                    MpM_FAIL_("Problem setting data track for database.");
                                    cout << "Data track not set for database." << endl;
                                }
                                break;

                            case 'e' :
                            case 'E' :
                                cout << "set e-mail address: ";
                                cout.flush();
                                getline(cin, inputLine);
                                inputString = inputLine.c_str();
                                if (aClient->setEmailAddressForDb(inputString))
                                {
                                    cout << "E-mail address set for database." << endl;
                                }
                                else
                                {
                                    ODL_LOG("! (aClient->" //####
                                            "setEmailAddressForDb(inputString))"); //####
                                    MpM_FAIL_("Problem setting e-mail address for database.");
                                    cout << "E-mail address not set for database." <<
                                    endl;
                                }
                                break;

                            case 'q' :
                            case 'Q' :
                                cout << "Exiting" << endl;
                                cout.flush();
                                if (! aClient->stopDbConnection())
                                {
                                    ODL_LOG("(! aClient->stopDbConnection())"); //####
                                    MpM_FAIL_("Problem stopping the database connection.");
                                }
                                StopRunning();
                                break;

                            case '?' :
                                // Help
                                displayCommands();
                                break;

                            default :
                                cout << "Unrecognized request '" << inChar << "'." << endl;
                                cout.flush();
                                break;

                        }
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
            ODL_LOG("! (aClient->findService(\"name:MovementDb\"))"); //####
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

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief The entry point for communicating with the Movement Database service.

 Integers read from standard input will be sent to the service as the number of requests to
 simulate. Entering a zero will exit the program.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the application.
 @returns @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
#if MAC_OR_LINUX_
# pragma unused(argc)
#endif // MAC_OR_LINUX_
    YarpString progName(*argv);

    ODL_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport | //####
             kODLoggingOptionWriteToStderr); //####
    ODL_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(progName);
#endif // MAC_OR_LINUX_
    try
    {
        Utilities::DescriptorVector argumentList;
        OutputFlavour               flavour;

        if (Utilities::ProcessStandardClientOptions(argc, argv, argumentList,
                                                    "The client for the Movement Database service",
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
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_
