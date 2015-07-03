//--------------------------------------------------------------------------------------------------
//
//  File:       m+mTestSourceMain.cpp
//
//  Project:    m+m
//
//  Contains:   The main application for the Test Source utility.
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
//  Created:    2015-02-20
//
//--------------------------------------------------------------------------------------------------

#include <m+m/m+mPortArgumentDescriptor.h>
#include <m+m/m+mUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file 
 @brief The main application for the Test Source utility. */

/*! @dir TestSource
 @brief The set of files that implement the Test Source utility. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The size of the buffer to be used for outgoing data. */
#define OUTGOING_SIZE_ 10240

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief The entry point for communicating with the Test Sink utility.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Test Source utility.
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
    try
    {
        int                               listenPort;
        Utilities::PortArgumentDescriptor firstArg("port", "The outgoing port", 12345, false, false,
                                                   &listenPort);
        Utilities::DescriptorVector       argumentList;
        OutputFlavour                     flavour; // ignored
        
        argumentList.push_back(&firstArg);
        if (Utilities::ProcessStandardUtilitiesOptions(argc, argv, argumentList,
                                                       T_("Communicates with the Test Sink "
                                                          "application"), 2015,
                                                       STANDARD_COPYRIGHT_NAME_, flavour, true))
        {
            Utilities::SetUpGlobalStatusReporter();
            try
            {
                SOCKET  listenSocket;
#if ! MAC_OR_LINUX_
                WORD    wVersionRequested = MAKEWORD(2, 2);
                WSADATA ww;
#endif // ! MAC_OR_LINUX_
                
#if MAC_OR_LINUX_
                listenSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
                if (INVALID_SOCKET != listenSocket)
                {
                    struct sockaddr_in addr;
                    
                    memset(&addr, 0, sizeof(addr));
                    addr.sin_family = AF_INET;
                    addr.sin_port = htons(listenPort);
                    addr.sin_addr.s_addr = htonl(INADDR_ANY);
                    if (bind(listenSocket, reinterpret_cast<struct sockaddr *>(&addr),
                             sizeof(addr)))
                    {
                        close(listenSocket);
                        listenSocket = INVALID_SOCKET;
                    }
                    else
                    {
                        listen(listenSocket, SOMAXCONN);
                    }
                }
#else // ! MAC_OR_LINUX_
                if (WSAStartup(wVersionRequested, &ww))
                {
                }
                else if ((2 == LOBYTE(ww.wVersion)) && (2 == HIBYTE(ww.wVersion)))
                {
                    listenSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
                    if (INVALID_SOCKET != listenSocket)
                    {
                        SOCKADDR_IN addr;
                        
                        addr.sin_family = AF_INET;
                        addr.sin_port = htons(listenPort);
                        addr.sin_addr.s_addr = htonl(INADDR_ANY);
                        int res = bind(listenSocket, reinterpret_cast<LPSOCKADDR>(&addr),
                                       sizeof(addr));
                        
                        if (SOCKET_ERROR == res)
                        {
                            OD_LOG("(SOCKET_ERROR == res)"); //####
                            closesocket(listenSocket);
                            listenSocket = INVALID_SOCKET;
                        }
                        else
                        {
                            listen(listenSocket, SOMAXCONN);
                        }
                    }
                }
#endif // ! MAC_OR_LINUX_
                if (INVALID_SOCKET != listenSocket)
                {
                    OD_LOG("(INVALID_SOCKET != listenSocket)"); //####
                    bool   keepGoing = true;
                    char   outBuff[OUTGOING_SIZE_];
                    OD_LOG("waiting for a connection"); //####
                    SOCKET sourceSocket = accept(listenSocket, nullptr, nullptr);
                    
                    OD_LOG_L1("sourceSocket = ", sourceSocket); //####
                    for (int ii = 0; ii < OUTGOING_SIZE_; ++ii)
                    {
                        outBuff[ii] = static_cast<char>(rand());
                    }
                    for ( ; keepGoing; )
                    {
#if MAC_OR_LINUX_
                        int outSize = std::max(1, rand() % OUTGOING_SIZE_);
#else // ! MAC_OR_LINUX_
                        int outSize = max(1, rand() % OUTGOING_SIZE_);
#endif // ! MAC_OR_LINUX_
                        
                        if (send(sourceSocket, outBuff, outSize, 0) != outSize)
                        {
                            OD_LOG("(send(sourceSocket, outBuff, outSize, 0) != outSize)"); //####
                            keepGoing = false;
                        }
                        else
                        {
                            cout << "sent " << outSize << " bytes." << endl;
                        }
                    }
#if MAC_OR_LINUX_
                    shutdown(listenSocket, SHUT_RDWR);
                    shutdown(sourceSocket, SHUT_RDWR);
                    close(listenSocket);
                    close(sourceSocket);
#else // ! MAC_OR_LINUX_
                    shutdown(listenSocket, SD_BOTH);
                    shutdown(sourceSocket, SD_BOTH);
                    closesocket(listenSocket);
                    closesocket(sourceSocket);
#endif // ! MAC_OR_LINUX_
                }
            }
            catch (...)
            {
                OD_LOG("Exception caught"); //####
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
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_
