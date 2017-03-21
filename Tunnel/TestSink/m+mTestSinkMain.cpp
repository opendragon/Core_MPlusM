//--------------------------------------------------------------------------------------------------
//
//  File:       m+mTestSinkMain.cpp
//
//  Project:    m+m
//
//  Contains:   The main application for the Test Sink utility.
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

#include <m+m/m+mAddressArgumentDescriptor.hpp>
#include <m+m/m+mPortArgumentDescriptor.hpp>
#include <m+m/m+mUtilities.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

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

/*! @brief The size of the buffer to be used for incoming data. */
#define INCOMING_SIZE_ 10240

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the Test Sink utility.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the Test Sink utility.
 @returns @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
#if defined(MpM_ServicesLogToStandardError)
    ODL_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
             kODLoggingOptionWriteToStderr | kODLoggingOptionEnableThreadSupport); //####
#else // ! defined(MpM_ServicesLogToStandardError)
    ODL_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
             kODLoggingOptionEnableThreadSupport); //####
#endif // ! defined(MpM_ServicesLogToStandardError)
    ODL_ENTER(); //####
    try
    {
        struct in_addr                       addrBuff;
        Utilities::AddressArgumentDescriptor firstArg("hostname", T_("IP address to connect to"),
                                                      Utilities::kArgModeRequired,
                                                      SELF_ADDRESS_IPADDR_, &addrBuff);
        Utilities::PortArgumentDescriptor    secondArg("port", T_("Port to connect to"),
                                                       Utilities::kArgModeRequired, 12345, true);
        Utilities::DescriptorVector          argumentList;
        OutputFlavour                        flavour; // ignored

        argumentList.push_back(&firstArg);
        argumentList.push_back(&secondArg);
        if (Utilities::ProcessStandardUtilitiesOptions(argc, argv, argumentList,
                                                       "The Test Sink application", 2015,
                                                       STANDARD_COPYRIGHT_NAME_, flavour, true))
        {
            bool       okSoFar;
#if (! MAC_OR_LINUX_)
            WORD       wVersionRequested = MAKEWORD(2, 2);
            WSADATA    ww;
#endif // ! MAC_OR_LINUX_

            Utilities::SetUpGlobalStatusReporter();
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

                // Useable data.
                char    buffer[INCOMING_SIZE_ + 100];
                SOCKET  sinkSocket;

                sinkSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
                if (INVALID_SOCKET != sinkSocket)
                {
                    int                hostPort = secondArg.getCurrentValue();
#if MAC_OR_LINUX_
                    struct sockaddr_in addr;
#else // ! MAC_OR_LINUX_
                    SOCKADDR_IN        addr;
#endif // ! MAC_OR_LINUX_

#if MAC_OR_LINUX_
                    memset(&addr, 0, sizeof(addr));
                    addr.sin_family = AF_INET;
                    addr.sin_port = htons(hostPort);
                    memcpy(&addr.sin_addr.s_addr, &addrBuff.s_addr,
                           sizeof(addr.sin_addr.s_addr));
                    ODL_LOG("connecting to source"); //####
                    if (connect(sinkSocket, reinterpret_cast<struct sockaddr *>(&addr),
                                sizeof(addr)))
                    {
                        close(sinkSocket);
                        sinkSocket = INVALID_SOCKET;
                    }
#else // ! MAC_OR_LINUX_
                    addr.sin_family = AF_INET;
                    addr.sin_port = htons(hostPort);
                    memcpy(&addr.sin_addr.s_addr, &addrBuff.s_addr,
                           sizeof(addr.sin_addr.s_addr));
                    ODL_LOG("connecting to source"); //####
                    int res = connect(sinkSocket, reinterpret_cast<LPSOCKADDR>(&addr),
                                      sizeof(addr));

                    if (SOCKET_ERROR == res)
                    {
                        closesocket(sinkSocket);
                        sinkSocket = INVALID_SOCKET;
                    }
#endif // ! MAC_OR_LINUX_
                }
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
                        ODL_LOG("! (0 < inSize)"); //####
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
            Utilities::ShutDownGlobalStatusReporter();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
    }
    ODL_EXIT_L(0); //####
    return 0;
} // main
