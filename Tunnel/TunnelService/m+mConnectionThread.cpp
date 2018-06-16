//--------------------------------------------------------------------------------------------------
//
//  File:       m+mConnectionThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a network connection handling thread.
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
//  Created:    2015-02-12
//
//--------------------------------------------------------------------------------------------------

#include "m+mConnectionThread.hpp"
#include "m+mTunnelService.hpp"

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a network connection handling thread. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Tunnel;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Create a 'listen' socket.
 @returns The new network socket on sucess or @c INVALID_SOCKET on failure. */
static SOCKET
createListener(void)
{
    ODL_ENTER(); //####
    SOCKET  listenSocket;
#if (! MAC_OR_LINUX_)
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
        addr.sin_addr.s_addr = htonl(INADDR_ANY);
        if (bind(listenSocket, reinterpret_cast<struct sockaddr *>(&addr), sizeof(addr)))
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
            addr.sin_addr.s_addr = htonl(INADDR_ANY);
            int res = bind(listenSocket, reinterpret_cast<LPSOCKADDR>(&addr), sizeof(addr));

            if (SOCKET_ERROR == res)
            {
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
    ODL_EXIT_L(listenSocket); //####
    return listenSocket;
} // createListener

/*! @brief Connect to the %Tunnel service 'raw' network port.
 @param[in] dataAddress The IP address to connect to.
 @param[in] dataPort The port number to connect to.
 @returns The new network socket on sucess or @c INVALID_SOCKET on failure. */
static SOCKET
connectToSource(const YarpString & dataAddress,
                const int          dataPort)
{
    ODL_ENTER(); //####
    ODL_S1s("dataAddress = ", dataAddress); //####
    ODL_L1("dataPort = ", dataPort); //####
    SOCKET         dataSocket = INVALID_SOCKET;
    struct in_addr addrBuff;
#if MAC_OR_LINUX_
    int            res = inet_pton(AF_INET, dataAddress.c_str(), &addrBuff);
#else // ! MAC_OR_LINUX_
    int            res = InetPton(AF_INET, dataAddress.c_str(), &addrBuff);
#endif // ! MAC_OR_LINUX_

    if (0 < res)
    {
        dataSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if (INVALID_SOCKET != dataSocket)
        {
#if MAC_OR_LINUX_
            struct sockaddr_in addr;
#else // ! MAC_OR_LINUX_
            SOCKADDR_IN        addr;
#endif // ! MAC_OR_LINUX_

#if MAC_OR_LINUX_
            memset(&addr, 0, sizeof(addr));
            addr.sin_family = AF_INET;
            addr.sin_port = htons(dataPort);
            memcpy(&addr.sin_addr.s_addr, &addrBuff.s_addr, sizeof(addr.sin_addr.s_addr));
            if (connect(dataSocket, reinterpret_cast<struct sockaddr *>(&addr), sizeof(addr)))
            {
                close(dataSocket);
                dataSocket = INVALID_SOCKET;
            }
#else // ! MAC_OR_LINUX_
            addr.sin_family = AF_INET;
            addr.sin_port = htons(dataPort);
            memcpy(&addr.sin_addr.s_addr, &addrBuff.s_addr, sizeof(addr.sin_addr.s_addr));
            int res = connect(dataSocket, reinterpret_cast<LPSOCKADDR>(&addr), sizeof(addr));

            if (SOCKET_ERROR == res)
            {
                closesocket(dataSocket);
                dataSocket = INVALID_SOCKET;
            }
#endif // ! MAC_OR_LINUX_
        }
    }
    ODL_EXIT_L(dataSocket); //####
    return dataSocket;
} // connectToSource

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

ConnectionThread::ConnectionThread(TunnelService & service) :
    inherited(), _service(service), _sourceAddress(""), _sourcePort(-1),
    _listenSocket(INVALID_SOCKET), _sourceSocket(INVALID_SOCKET)
{
    ODL_ENTER(); //####
    ODL_P1("service = ", &service); //####
    ODL_EXIT_P(this); //####
} // ConnectionThread::ConnectionThread

ConnectionThread::~ConnectionThread(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // ConnectionThread::~ConnectionThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
ConnectionThread::run(void)
{
    ODL_OBJENTER(); //####
    SOCKET destinationSocket = accept(_listenSocket, 0, 0);

    if (INVALID_SOCKET == destinationSocket)
    {
        _service.setPort(-1);
#if MAC_OR_LINUX_
        shutdown(_listenSocket, SHUT_RDWR);
        close(_listenSocket);
#else // ! MAC_OR_LINUX_
        shutdown(_listenSocket, SD_BOTH);
        closesocket(_listenSocket);
#endif // ! MAC_OR_LINUX_
    }
    else
    {
        ODL_LOG("! (INVALID_SOCKET == destinationSocket)"); //#####
        _sourceSocket = connectToSource(_sourceAddress, _sourcePort);
        if (INVALID_SOCKET == _sourceSocket)
        {
#if MAC_OR_LINUX_
            close(_sourceSocket);
#else // ! MAC_OR_LINUX_
            closesocket(_sourceSocket);
#endif // ! MAC_OR_LINUX_
        }
        else
        {
            char buffer[10240];

            for (bool keepGoing = true; keepGoing && (! isStopping()); )
            {
                ConsumeSomeTime();
#if MAC_OR_LINUX_
                ssize_t inSize = recv(_sourceSocket, buffer, sizeof(buffer), 0);
#else // ! MAC_OR_LINUX_
                int     inSize = recv(_sourceSocket, buffer, sizeof(buffer), 0);
#endif // ! MAC_OR_LINUX_

                if (0 < inSize)
                {
                    if (send(destinationSocket, buffer, inSize, 0) == inSize)
                    {
                        Common::SendReceiveCounters newCount(inSize, 1, inSize, 1);

                        _service.incrementAuxiliaryCounters(newCount);
                    }
                    else
                    {
                        ODL_LOG("! (send(destinationSocket, buffer, inSize, 0) == inSize)"); //####
                        keepGoing = false;
                    }
                }
                else
                {
                    ODL_LOG("! (0 < inSize)"); //####
                    keepGoing = false;
                }
            }
            _service.setPort(-1);
#if MAC_OR_LINUX_
            shutdown(destinationSocket, SHUT_RDWR);
            shutdown(_listenSocket, SHUT_RDWR);
            shutdown(_sourceSocket, SHUT_RDWR);
            close(destinationSocket);
            close(_listenSocket);
            close(_sourceSocket);
#else // ! MAC_OR_LINUX_
            shutdown(destinationSocket, SD_BOTH);
            shutdown(_listenSocket, SD_BOTH);
            shutdown(_sourceSocket, SD_BOTH);
            closesocket(destinationSocket);
            closesocket(_listenSocket);
            closesocket(_sourceSocket);
#endif // ! MAC_OR_LINUX_
            StopRunning();
        }
    }
    ODL_OBJEXIT(); //####
} // ConnectionThread::run

void
ConnectionThread::setSourceAddress(const YarpString & sourceName,
                                   const int          sourcePort)
{
    ODL_OBJENTER(); //####
    ODL_S1s("sourceName = ", sourceName); //####
    ODL_L1("sourcePort = ", sourcePort); //####
    YarpString tunnelAddress;
    int        tunnelPort;

    _sourceAddress = sourceName;
    _sourcePort = sourcePort;
    // We'll be determining the port to use, so the value returned for the port, here, is ignored.
    _service.getAddress(tunnelAddress, tunnelPort);
    _listenSocket = createListener();
    if (INVALID_SOCKET != _listenSocket)
    {
        struct sockaddr_in sin;
        socklen_t          len = sizeof(sin);

        if ((! getsockname(_listenSocket, reinterpret_cast<struct sockaddr *>(&sin), &len)) &&
            (sin.sin_family == AF_INET) && (sizeof(sin) == len))
        {
            _service.setPort(ntohs(sin.sin_port));
        }
        else
        {
#if MAC_OR_LINUX_
            close(_listenSocket);
            _listenSocket = INVALID_SOCKET;
#else // ! MAC_OR_LINUX_
            closesocket(_listenSocket);
            _listenSocket = INVALID_SOCKET;
#endif // ! MAC_OR_LINUX_
        }
    }
    ODL_OBJEXIT(); //####
} // ConnectionThread::setSourceAddress

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
