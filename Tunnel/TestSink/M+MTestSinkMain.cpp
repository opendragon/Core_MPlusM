//--------------------------------------------------------------------------------------------------
//
//  File:       M+MTestSinkMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the Test Sink utility.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by HPlus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2015-02-20
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
 @brief The main application for the Test Sink utility. */

/*! @dir TestSink
 @brief The set of files that implement the Test Sink utility. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#define INCOMING_SIZE 10240

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the Test Sink utility.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Test Sink utility.
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

        if (Utilities::ProcessStandardUtilitiesOptions(argc, argv,
                                                       T_(" hostname port\n\n"
                                                          "  hostname   IP address to connect to\n"
                                                          "  port       port to connect to"),
                                                       flavour, &arguments))
        {
            if (2 <= arguments.size())
            {
                struct in_addr        addrBuff;
                int                   sourcePort = -1;
                yarp::os::ConstString sourceName;
                const char *          startPtr = arguments[1].c_str();
                char *                endPtr;
                int                   tempInt = static_cast<int>(strtol(startPtr, &endPtr, 10));

                sourceName = arguments[0];
                OD_LOG_S1s("sourceName <- ", sourceName); //####
                if ((0 < inet_pton(AF_INET, sourceName.c_str(), &addrBuff)) &&
                    (startPtr != endPtr) && (! *endPtr) && (0 < tempInt))
                {
                    // Useable data.
                    sourcePort = tempInt;
                }
                if ((0 < sourcePort) && (0 < sourceName.size()))
                {
                    char           buffer[INCOMING_SIZE + 100];
                    int            res;
                    struct in_addr addrBuff;
                    SOCKET         sinkSocket;
#if (! MAC_OR_LINUX_)
                    WORD           wVersionRequested = MAKEWORD(2, 2);
                    WSADATA        ww;
#endif // ! MAC_OR_LINUX_

#if MAC_OR_LINUX_
                    res = inet_pton(AF_INET, sourceName.c_str(), &addrBuff);
                    if (0 < res)
                    {
                        sinkSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
                        if (INVALID_SOCKET != sinkSocket)
                        {
                            struct sockaddr_in addr;

                            memset(&addr, 0, sizeof(addr));
                            addr.sin_family = AF_INET;
                            addr.sin_port = htons(sourcePort);
                            memcpy(&addr.sin_addr.s_addr, &addrBuff.s_addr,
                                   sizeof(addr.sin_addr.s_addr));
                            OD_LOG("connecting to source"); //####
                            if (connect(sinkSocket, reinterpret_cast<struct sockaddr *>(&addr),
                                        sizeof(addr)))
                            {
                                close(sinkSocket);
                                sinkSocket = INVALID_SOCKET;
                            }
                        }
                    }
#else // ! MAC_OR_LINUX_
                    if (WSAStartup(wVersionRequested, &ww))
                    {
                    }
                    else if ((2 == LOBYTE(ww.wVersion)) && (2 == HIBYTE(ww.wVersion)))
                    {
                        res = InetPton(AF_INET, sourceName.c_str(), &addrBuff);
                        if (0 < res)
                        {
                            sinkSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
                            if (INVALID_SOCKET != sinkSocket)
                            {
                                SOCKADDR_IN addr;

                                addr.sin_family = AF_INET;
                                addr.sin_port = htons(sourcePort);
                                memcpy(&addr.sin_addr.s_addr, &addrBuff.s_addr,
                                       sizeof(addr.sin_addr.s_addr));
                                OD_LOG("connecting to source"); //####
                                res = connect(sinkSocket, reinterpret_cast<LPSOCKADDR>(&addr),
                                              sizeof(addr));
                                if (SOCKET_ERROR == res)
                                {
                                    closesocket(sinkSocket);
                                    sinkSocket = INVALID_SOCKET;
                                }
                            }
                        }
                    }
#endif // ! MAC_OR_LINUX_
                    for (bool keepGoing = true; keepGoing; )
                    {
#if MAC_OR_LINUX_
                        ssize_t inSize = recv(sinkSocket, buffer, sizeof(buffer), 0);
#else // ! MAC_OR_LINUX_
                        int     inSize = recv(sinkSocket, buffer, sizeof(buffer), 0);
#endif // ! MAC_OR_LINUX_
                        
                        if (0 < inSize)
                        {
                            cout << "received " << inSize << " bytes." << endl;
                        }
                        else
                        {
                            OD_LOG("! (0 < inSize)"); //####
                            keepGoing = false;
                        }
                    }
#if MAC_OR_LINUX_
                    shutdown(sinkSocket, SHUT_RDWR);
                    close(sinkSocket);
#else // ! MAC_OR_LINUX_
                    shutdown(sinkSocket, SD_BOTH);
                    closesocket(sinkSocket);
#endif // ! MAC_OR_LINUX_
                }
                else
                {
                    cerr << "Invalid argument(s)." << endl;
                }
            }
            else
            {
                cerr << "Missing argument(s)." << endl;
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
