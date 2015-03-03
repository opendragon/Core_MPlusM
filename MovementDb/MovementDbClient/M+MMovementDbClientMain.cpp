//--------------------------------------------------------------------------------------------------
//
//  File:       MovementDbClientMain.cpp
//
//  Project:    M+M
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

#include "M+MMovementDbClient.h"

#include <mpm/M+MUtilities.h>

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
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Display the available commands. */
static void displayCommands(void)
{
    OD_LOG_ENTER(); //####
    cout << "Commands:" << endl;
    cout << "  ? - display this list" << endl;
    cout << "  + - request a file path adn add it to the database" << endl;
    cout << "  d - set the data track" << endl;
    cout << "  e - set the e-mail address" << endl;
    cout << "  q - quit the application" << endl;
    OD_LOG_EXIT(); //####
} // displayCommands

/*! @brief Write out a time value in a human-friendly form.
 @param measurement The time value to write out. */
static void reportTimeInReasonableUnits(const double measurement)
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
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
#if MAC_OR_LINUX_
# pragma unused(argc)
#endif // MAC_OR_LINUX_
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(*argv);
#endif // MAC_OR_LINUX_
    try
    {
        Utilities::SetUpGlobalStatusReporter();
#if defined(MpM_ReportOnConnections)
        ChannelStatusReporter * reporter = Utilities::GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)

        if (CanReadFromStandardInput())
        {
            Utilities::CheckForNameServerReporter();
#if CheckNetworkWorks_
            if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))
#endif // CheckNetworkWorks_
            {
                yarp::os::Network yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure
                
                Initialize(*argv);
                MovementDbClient * stuff = new MovementDbClient;
                
                if (stuff)
                {
#if defined(MpM_ReportOnConnections)
                    stuff->setReporter(reporter, true);
#endif // defined(MpM_ReportOnConnections)
                    StartRunning();
                    SetSignalHandlers(SignalRunningStop);
                    if (stuff->findService("name:MovementDb"))
                    {
                        if (stuff->connectToService())
                        {
                            for ( ; IsRunning(); )
                            {
                                char                  inChar;
                                std::string           inputLine;
                                yarp::os::ConstString inputString;
                                
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
                                                if (stuff->addFileToDb(inputString))
                                                {
                                                    cout << "File added to database." << endl;
                                                }
                                                else
                                                {
                                                    OD_LOG("! (stuff->addFileToDb(" //####
                                                           "inputString))"); //####
#if MAC_OR_LINUX_
                                                    GetLogger().fail("Problem adding file to "
                                                                     "database.");
#endif // MAC_OR_LINUX_
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
                                            if (stuff->setDataTrackForDb(inputString))
                                            {
                                                cout << "Data track set for database." << endl;
                                            }
                                            else
                                            {
                                                OD_LOG("! (stuff->setDataTrackForDb(" //####
                                                       "inputString))"); //####
#if MAC_OR_LINUX_
                                                GetLogger().fail("Problem setting data track for "
                                                                 "database.");
#endif // MAC_OR_LINUX_
                                                cout << "Data track not set for database." << endl;
                                            }
                                            break;
                                            
                                        case 'e' :
                                        case 'E' :
                                            cout << "set e-mail address: ";
                                            cout.flush();
                                            getline(cin, inputLine);
                                            inputString = inputLine.c_str();
                                            if (stuff->setEmailAddressForDb(inputString))
                                            {
                                                cout << "E-mail address set for database." << endl;
                                            }
                                            else
                                            {
                                                OD_LOG("! (stuff->setEmailAddressForDb(" //####
                                                       "inputString))"); //####
#if MAC_OR_LINUX_
                                                GetLogger().fail("Problem setting e-mail address "
                                                                 "for database.");
#endif // MAC_OR_LINUX_
                                                cout << "E-mail address not set for database." <<
                                                        endl;
                                            }
                                            break;
                                            
                                        case 'q' :
                                        case 'Q' :
                                            cout << "Exiting" << endl;
                                            cout.flush();
                                            if (! stuff->stopDbConnection())
                                            {
                                                OD_LOG("(! stuff->stopDbConnection())"); //####
#if MAC_OR_LINUX_
                                                GetLogger().fail("Problem stopping the database "
                                                                 "connection.");
#endif // MAC_OR_LINUX_
                                            }
                                            StopRunning();
                                            break;
                                            
                                        case '?' :
                                            // Help
                                            displayCommands();
                                            break;
                                            
                                        default :
                                            cout << "Unrecognized request '" << inChar << "'." <<
                                                    endl;
                                            cout.flush();
                                            break;
                                            
                                    }
                                }
                            }
                            if (! stuff->disconnectFromService())
                            {
                                OD_LOG("(! stuff->disconnectFromService())"); //####
#if MAC_OR_LINUX_
                                GetLogger().fail("Problem disconnecting from the service.");
#endif // MAC_OR_LINUX_
                            }
                        }
                        else
                        {
                            OD_LOG("! (stuff->connectToService())"); //####
#if MAC_OR_LINUX_
                            GetLogger().fail("Could not connect to the required service.");
#else // ! MAC_OR_LINUX_
                            cerr << "Could not connect to the required service." << endl;
#endif // ! MAC_OR_LINUX_
                        }
                    }
                    else
                    {
                        OD_LOG("! (stuff->findService(\"name:MovementDb\"))"); //####
#if MAC_OR_LINUX_
                        GetLogger().fail("Could not find the required service.");
#else // ! MAC_OR_LINUX_
                        cerr << "Could not find the required service." << endl;
#endif // ! MAC_OR_LINUX_
                    }
                    delete stuff;
                }
                else
                {
                    OD_LOG("! (stuff)"); //####
                }
            }
#if CheckNetworkWorks_
            else
            {
                OD_LOG("! (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))"); //####
# if MAC_OR_LINUX_
                GetLogger().fail("YARP network not running.");
# else // ! MAC_OR_LINUX_
                cerr << "YARP network not running." << endl;
# endif // ! MAC_OR_LINUX_
            }
#endif // CheckNetworkWorks_
        }
        Utilities::ShutDownGlobalStatusReporter();
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
