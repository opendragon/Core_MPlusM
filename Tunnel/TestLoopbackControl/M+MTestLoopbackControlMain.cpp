//--------------------------------------------------------------------------------------------------
//
//  File:       M+MTestLoopbackControlMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the Test Loopback Control utility.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2015-02-26
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the Test Loopback Control utility. */

/*! @dir TestLoopbackControl
 @brief The set of files that implement the Test Loopback Control utility. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cerr;
using std::cin;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The size of the receive / send buffer. */
#define BUFFER_SIZE 1024

#if (! MAC_OR_LINUX_)
/*! @brief The number of ticks per second. */
static LARGE_INTEGER lFrequency;
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

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

/*! @brief Display the available commands. */
static void displayCommands(void)
{
    OD_LOG_ENTER(); //####
    cout << "Commands:" << endl;
    cout << "  ? - display this list" << endl;
    cout << "  q - quit the application" << endl;
    cout << "  r - send some random numbers" << endl;
    cout << "  t - send some (entered in) text" << endl;
    OD_LOG_EXIT(); //####
} // displayCommands

/*! @brief Return the current epoch time in seconds.
@returns The time in seconds. */
double getTimeNow(void)
{
    double        result;
#if (! MAC_OR_LINUX_)
    LARGE_INTEGER now;
#endif // ! MAC_OR_LINUX_

#if MAC_OR_LINUX_
    result = yarp::os::Time::now();
#else // ! MAC_OR_LINUX_
    QueryPerformanceCounter(&now);
    result = ((1.0 * now.QuadPart) / lFrequency.QuadPart);
#endif // ! MAC_OR_LINUX_
    return result;
} // getTimeNow

/*! @brief Send and receive a random byte sequence.
 @param talkSocket The socket to use for communication. */
static void sendAndReceiveRandom(SOCKET talkSocket)
{
    OD_LOG_ENTER(); //####
    char inBuffer[BUFFER_SIZE];
    char outBuffer[BUFFER_SIZE];
#if MAC_OR_LINUX_
    int  sendSize = std::max(1, rand() % BUFFER_SIZE);
#else // ! MAC_OR_LINUX_
    int  sendSize = max(1, rand() % BUFFER_SIZE);
#endif // ! MAC_OR_LINUX_

    for (int ii = 0; ii < sendSize; ++ii)
    {
        outBuffer[ii] = static_cast<char>(rand());
    }
    double beforeSend = getTimeNow();

    if (send(talkSocket, outBuffer, sendSize, 0) == sendSize)
    {
        double afterSend = getTimeNow();
        
        cout << "sent " << sendSize << " bytes." << endl;
        double  beforeReceive = getTimeNow();
#if MAC_OR_LINUX_
        ssize_t inSize = recv(talkSocket, inBuffer, sizeof(inBuffer), 0);
#else // ! MAC_OR_LINUX_
        int     inSize = recv(talkSocket, inBuffer, sizeof(inBuffer), 0);
#endif // ! MAC_OR_LINUX_
        
        if (0 < inSize)
        {
            bool   okSoFar = true;
            double afterReceive = getTimeNow();
            
            cout << "received " << inSize << " bytes." << endl;
            for (int ii = 0; okSoFar && (ii < inSize); ++ii)
            {
                if (inBuffer[ii] != outBuffer[ii])
                {
                    cerr << "mismatch at byte " << ii << endl;
                    okSoFar = false;
                }
            }
            if (okSoFar)
            {
                double elapsedTime = (afterReceive - beforeReceive) + (afterSend - beforeSend);
                
                cout << "Elapsed time = ";
                reportTimeInReasonableUnits(elapsedTime);
                cout << endl;
            }
            else
            {
                StopRunning();
            }
        }
        else
        {
            OD_LOG("! (0 < inSize)"); //####
            StopRunning();
        }
    }
    else
    {
        OD_LOG("! (send(talkSocket, outBuffer, sendSize, 0) == sendSize)"); //####
        StopRunning();
    }
    OD_LOG_EXIT(); //####
} // sendAndReceiveRandom

/*! @brief Send and receive a text sequence.
 @param talkSocket The socket to use for communication.
 @param inputLine The text to be used. */
static void sendAndReceiveText(SOCKET              talkSocket,
                               const std::string & inputLine)
{
    OD_LOG_ENTER(); //####
    char         inBuffer[BUFFER_SIZE];
    int          inSize = inputLine.size();
    const char * inChars = inputLine.c_str();
#if MAC_OR_LINUX_
    int          sendSize = std::min(inSize, BUFFER_SIZE);
#else // ! MAC_OR_LINUX_
    int          sendSize = min(inSize, BUFFER_SIZE);
#endif // ! MAC_OR_LINUX_
    double       beforeSend = getTimeNow();
    
    if (send(talkSocket, inChars, sendSize, 0) == sendSize)
    {
        double afterSend = getTimeNow();
        
        cout << "sent " << sendSize << " bytes." << endl;
        double  beforeReceive = getTimeNow();
#if MAC_OR_LINUX_
        ssize_t inSize = recv(talkSocket, inBuffer, sizeof(inBuffer), 0);
#else // ! MAC_OR_LINUX_
        int     inSize = recv(talkSocket, inBuffer, sizeof(inBuffer), 0);
#endif // ! MAC_OR_LINUX_
        
        if (0 < inSize)
        {
            bool   okSoFar = true;
            double afterReceive = getTimeNow();
            
            cout << "received " << inSize << " bytes." << endl;
            for (int ii = 0; okSoFar && (ii < inSize); ++ii)
            {
                if (inBuffer[ii] != inChars[ii])
                {
                    cerr << "mismatch at byte " << ii << endl;
                    okSoFar = false;
                }
            }
            if (okSoFar)
            {
                double elapsedTime = (afterReceive - beforeReceive) + (afterSend - beforeSend);
                
                cout << "Elapsed time = ";
                reportTimeInReasonableUnits(elapsedTime);
                cout << endl;
            }
            else
            {
                StopRunning();
            }
        }
        else
        {
            OD_LOG("! (0 < inSize)"); //####
            StopRunning();
        }
    }
    else
    {
        OD_LOG("! (send(talkSocket, inChars, sendSize, 0) == sendSize)"); //####
        StopRunning();
    }
    OD_LOG_EXIT(); //####
} // sendAndReceiveText

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the Test Loopback Control application.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Test Loopback Control application.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
#if defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionWriteToStderr | kODLoggingOptionEnableThreadSupport); //####
#else // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport); //####
#endif // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_ENTER(); //####
    try
    {
        OutputFlavour flavour; // ignored
        StringVector  arguments;

        if (Utilities::ProcessStandardUtilitiesOptions(argc, argv, T_(" hostname port"),
                                                       T_("  hostname   IP address to connect to\n"
                                                          "  port       port to connect to"), 2015,
                                                       STANDARD_COPYRIGHT_NAME, flavour,
                                                       &arguments))
        {
			Utilities::SetUpGlobalStatusReporter();
			if (2 <= arguments.size())
            {
                if (CanReadFromStandardInput())
                {
                    bool                  okSoFar;
                    yarp::os::ConstString sourceName(arguments[0]);
#if (! MAC_OR_LINUX_)
                    WORD                  wVersionRequested = MAKEWORD(2, 2);
                    WSADATA               ww;
#endif // ! MAC_OR_LINUX_
                    
                    OD_LOG_S1s("sourceName <- ", sourceName); //####
#if MAC_OR_LINUX_
                    okSoFar = true;
#else // ! MAC_OR_LINUX_
                    if (WSAStartup(wVersionRequested, &ww))
                    {
                        okSoFar = false;
                    }
                    else if ((2 == LOBYTE(ww.wVersion)) && (2 == HIBYTE(ww.wVersion)))
                    {
                        okSoFar = true;
                    }
                    else
                    {
                        okSoFar = false;
                    }
#endif // ! MAC_OR_LINUX_
                    if (okSoFar)
                    {
                        struct in_addr addrBuff;
                        const char *   startPtr = arguments[1].c_str();
                        char *         endPtr;
                        int            tempInt = static_cast<int>(strtol(startPtr, &endPtr, 10));
#if MAC_OR_LINUX_
                        int            res = inet_pton(AF_INET, sourceName.c_str(), &addrBuff);
#else // ! MAC_OR_LINUX_
                        int            res = InetPton(AF_INET, sourceName.c_str(), &addrBuff);
#endif // ! MAC_OR_LINUX_
                        
                        if ((0 < res) && (startPtr != endPtr) && (! *endPtr) &&
                            Utilities::ValidPortNumber(tempInt))
                        {
                            // Useable data.
                            SOCKET talkSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
                            
                            if (INVALID_SOCKET != talkSocket)
                            {
#if MAC_OR_LINUX_
                                struct sockaddr_in addr;
#else // ! MAC_OR_LINUX_
                                SOCKADDR_IN        addr;
#endif // ! MAC_OR_LINUX_
                                
#if MAC_OR_LINUX_
                                memset(&addr, 0, sizeof(addr));
                                addr.sin_family = AF_INET;
                                addr.sin_port = htons(tempInt);
                                memcpy(&addr.sin_addr.s_addr, &addrBuff.s_addr,
                                       sizeof(addr.sin_addr.s_addr));
                                OD_LOG("connecting to source"); //####
                                if (connect(talkSocket, reinterpret_cast<struct sockaddr *>(&addr),
                                            sizeof(addr)))
                                {
                                    close(talkSocket);
                                    talkSocket = INVALID_SOCKET;
                                }
                                else
                                {
                                    OD_LOG("connected"); //####
                                }
#else // ! MAC_OR_LINUX_
                                addr.sin_family = AF_INET;
                                addr.sin_port = htons(tempInt);
                                memcpy(&addr.sin_addr.s_addr, &addrBuff.s_addr,
                                       sizeof(addr.sin_addr.s_addr));
                                OD_LOG("connecting to source"); //####
                                res = connect(talkSocket, reinterpret_cast<LPSOCKADDR>(&addr),
                                              sizeof(addr));
                                if (SOCKET_ERROR == res)
                                {
                                    closesocket(talkSocket);
                                    talkSocket = INVALID_SOCKET;
                                }
                                else
                                {
                                    OD_LOG("connected"); //####
                                }
#endif // ! MAC_OR_LINUX_
                            }
                            if (INVALID_SOCKET == talkSocket)
                            {
                                cerr << "Problem connecting to provided IP address or port." << endl;
                            }
                            else
                            {
#if (! MAC_OR_LINUX_)
                                QueryPerformanceFrequency(&lFrequency);
                                cout << lFrequency.QuadPart << endl;
#endif // ! MAC_OR_LINUX_
                                for (StartRunning(); IsRunning(); )
                                {
                                    char        inChar;
                                    std::string inputLine;
                                    
                                    cout << "Operation: [? + q r t]? ";
                                    cout.flush();
                                    if (getline(cin, inputLine))
                                    {
                                        inChar = 0;
                                        for (int ii = 0, len = inputLine.size(); ii < len; ++ii)
                                        {
                                            char aChar = inputLine[ii];
                                            
                                            if (! isspace(aChar))
                                            {
                                                inChar = aChar;
                                                break;
                                            }
                                            
                                        }
                                        switch (inChar)
                                        {
                                            case '?' :
                                                // Help
                                                displayCommands();
                                                break;
                                                
                                            case 'q' :
                                            case 'Q' :
                                                cout << "Exiting" << endl;
                                                StopRunning();
                                                break;
                                                
                                            case 'r' :
                                            case 'R' :
                                                sendAndReceiveRandom(talkSocket);
                                                break;
                                                
                                            case 't' :
                                            case 'T' :
                                                cout << "Type something to be echoed: ";
                                                cout.flush();
                                                if (getline(cin, inputLine))
                                                {
                                                    sendAndReceiveText(talkSocket, inputLine);
                                                }
                                                else
                                                {
                                                    StopRunning();
                                                }
                                                break;
                                                
                                        }
                                    }
                                    else
                                    {
                                        StopRunning();
                                    }
                                }
                            }
#if MAC_OR_LINUX_
                            shutdown(talkSocket, SHUT_RDWR);
                            close(talkSocket);
#else // ! MAC_OR_LINUX_
                            shutdown(talkSocket, SD_BOTH);
                            closesocket(talkSocket);
#endif // ! MAC_OR_LINUX_
                        }
                        else
                        {
                            cerr << "Invalid argument(s)." << endl;
                        }
                    }
                }
            }
            else
            {
                cerr << "Missing argument(s)." << endl;
            }
			Utilities::ShutDownGlobalStatusReporter();
		}
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
