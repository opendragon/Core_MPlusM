//--------------------------------------------------------------------------------------------------
//
//  File:       M+MTunnelClientMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the client of the Tunnel service.
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
//  Created:    2015-02-11
//
//--------------------------------------------------------------------------------------------------

#include "M+MTunnelClient.h"
#include "M+MTunnelRequests.h"

#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file 
 @brief The main application for the client of the Tunnel service. */

/*! @dir TunnelClient
 @brief The set of files that implement the Tunnel client. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Tunnel;
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

/*! @brief Process the argument list for the application.
 @param arguments The arguments to analyze.
 @param namePattern The generated search value. */
static void processArguments(const StringVector &    arguments,
                             yarp::os::ConstString & namePattern)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("arguments = ", &arguments, "namePattern = ", &namePattern); //####
    yarp::os::ConstString tag;
    
    for (int ii = 0, argc = arguments.size(); argc > ii; ++ii)
    {
        tag = arguments[ii];
    }
    if (0 < tag.length())
    {
        yarp::os::ConstString singleQuote("'");

        namePattern = singleQuote + namePattern + " " + tag + singleQuote;
    }
    OD_LOG_EXIT(); //####
} // processArguments

/*! @brief Create a 'listen' socket.
 @param listenPort The network port to attach the new socket to.
 @returns The new network socket on sucess or @c INVALID_SOCKET on failure. */
static SOCKET setUpListeningPost(const int listenPort)
{
    OD_LOG_ENTER(); //####
    OD_LOG_L1("listenPort = ", listenPort); //####
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
            addr.sin_port = htons(listenPort);
            addr.sin_addr.s_addr = htonl(INADDR_ANY);
            int res = bind(listenSocket, reinterpret_cast<LPSOCKADDR>(&addr), sizeof(addr));
            
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
    OD_LOG_EXIT_L(listenSocket); //####
    return listenSocket;
} // setUpListeningPost

/*! @brief Connect to the Tunnel service 'raw' network port.
 @param serviceAddress The IP address to connect to.
 @param servicePort The port number to connect to.
 @returns The new network socket on sucess or @c INVALID_SOCKET on failure. */
static SOCKET connectToTunnel(const yarp::os::ConstString & serviceAddress,
                              const int                     servicePort)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceAddress = ", serviceAddress); //####
    OD_LOG_L1("servicePort = ", servicePort); //####
    SOCKET         tunnelSocket = INVALID_SOCKET;
    struct in_addr addrBuff;
#if MAC_OR_LINUX_
    int            res = inet_pton(AF_INET, serviceAddress.c_str(), &addrBuff);
#else // ! MAC_OR_LINUX_
    int            res = InetPton(AF_INET, serviceAddress.c_str(), &addrBuff);
#endif // ! MAC_OR_LINUX_
    
    if (0 < res)
    {
        tunnelSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        OD_LOG_L1("tunnelSocket = ", tunnelSocket); //####
#if MAC_OR_LINUX_
        if (INVALID_SOCKET != tunnelSocket)
        {
            struct sockaddr_in addr;
            
            memset(&addr, 0, sizeof(addr));
            addr.sin_family = AF_INET;
            addr.sin_port = htons(servicePort);
            memcpy(&addr.sin_addr.s_addr, &addrBuff.s_addr, sizeof(addr.sin_addr.s_addr));
            if (connect(tunnelSocket, reinterpret_cast<struct sockaddr *>(&addr), sizeof(addr)))
            {
                OD_LOG("(connect(tunnelSocket, reinterpret_cast<struct sockaddr *>(&addr), " //####
                       "sizeof(addr)))"); //####
                close(tunnelSocket);
                tunnelSocket = INVALID_SOCKET;
            }
        }
#else // ! MAC_OR_LINUX_
        if (INVALID_SOCKET != tunnelSocket)
        {
            SOCKADDR_IN addr;
            
            addr.sin_family = AF_INET;
            addr.sin_port = htons(servicePort);
            memcpy(&addr.sin_addr.s_addr, &addrBuff.s_addr, sizeof(addr.sin_addr.s_addr));
            int res = connect(tunnelSocket, reinterpret_cast<LPSOCKADDR>(&addr), sizeof(addr));
            
            if (SOCKET_ERROR == res)
            {
                OD_LOG("(SOCKET_ERROR == res)"); //####
                closesocket(tunnelSocket);
                tunnelSocket = INVALID_SOCKET;
            }
        }
#endif // ! MAC_OR_LINUX_
    }
    OD_LOG_EXIT_L(tunnelSocket); //####
    return tunnelSocket;
} // connectToTunnel

/*! @brief Handle the network connections.
 @param listenSocket The 'listen' socket to use.
 @param serviceAddress The IP address to connect to.
 @param servicePort The port number to connect to. */
static void handleConnections(SOCKET                        listenSocket,
                              const yarp::os::ConstString & serviceAddress,
                              const int                     servicePort)
{
    OD_LOG_ENTER(); //####
    OD_LOG_L2("listenSocket = ", listenSocket, "servicePort = ", servicePort); //####
    OD_LOG_S1s("serviceAddress = ", serviceAddress); //####
    bool   keepGoing = true;
    char   buffer[10240];
    SOCKET sinkSocket = accept(listenSocket, NULL, NULL);

    OD_LOG_L1("sinkSocket = ", sinkSocket); //####
    if (INVALID_SOCKET != sinkSocket)
    {
        SOCKET tunnelSocket = connectToTunnel(serviceAddress, servicePort);
        
        if (INVALID_SOCKET != tunnelSocket)
        {
            OD_LOG("(INVALID_SOCKET != tunnelSocket)"); //####
            for ( ; keepGoing; )
            {
                yarp::os::Time::yield();
#if MAC_OR_LINUX_
                ssize_t inSize = recv(tunnelSocket, buffer, sizeof(buffer), 0);
#else // ! MAC_OR_LINUX_
                int     inSize = recv(tunnelSocket, buffer, sizeof(buffer), 0);
#endif // ! MAC_OR_LINUX_
                
                if (0 < inSize)
                {
                    if (send(sinkSocket, buffer, inSize, 0) != inSize)
                    {
                        OD_LOG("(send(sinkSocket, buffer, inSize, 0) != inSize)"); //####
                        keepGoing = false;
                    }
                }
                else
                {
                    OD_LOG("! (0 < inSize)"); //####
                    keepGoing = false;
                }
            }
#if MAC_OR_LINUX_
            shutdown(tunnelSocket, SHUT_RDWR);
            close(tunnelSocket);
#else // ! MAC_OR_LINUX_
            shutdown(tunnelSocket, SD_BOTH);
            closesocket(tunnelSocket);
#endif // ! MAC_OR_LINUX_
        }
#if MAC_OR_LINUX_
        shutdown(sinkSocket, SHUT_RDWR);
        close(sinkSocket);
#else // ! MAC_OR_LINUX_
        shutdown(sinkSocket, SD_BOTH);
        closesocket(sinkSocket);
#endif // ! MAC_OR_LINUX_
    }
    OD_LOG_EXIT(); //####
} // handleConnections

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief The entry point for communicating with the Tunnel service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Tunnel client.
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
        OutputFlavour           flavour; // ignored
        StringVector            arguments;
        
        if (Utilities::ProcessStandardUtilitiesOptions(argc, argv,
                                                       " port [tag]\n\n"
                                                       "  port       The outgoing port\n"
                                                       "  tag        Optional tag for the service "
                                                       "to be connnected to", flavour, &arguments))
        {
            yarp::os::ConstString namePattern(MpM_TUNNEL_CANONICAL_NAME);
            int                   listenPort = -1;
            
            if (1 <= arguments.size())
            {
                const char * startPtr = arguments[0].c_str();
                char *       endPtr;
                int          tempInt = static_cast<int>(strtol(startPtr, &endPtr, 10));

                if ((startPtr != endPtr) && (! *endPtr) && (1024 <= tempInt))
                {
                    // Useable data.
                    listenPort = tempInt;
                }
                if (1 < arguments.size())
                {
                    yarp::os::ConstString tag(arguments[1]);
                    
                    if (0 < tag.length())
                    {
                        yarp::os::ConstString singleQuote("'");

                        namePattern = singleQuote + namePattern + " " + tag + singleQuote;
                    }
                }            
            }
            else
            {
#if MAC_OR_LINUX_
                GetLogger().fail("Missing argument(s).");
#else // ! MAC_OR_LINUX_
                cerr << "Missing argument(s)." << endl;
#endif // ! MAC_OR_LINUX_
            }
            if (0 < listenPort)
            {
                try
                {
                    Utilities::CheckForNameServerReporter();
#if CheckNetworkWorks_
                    if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))
#endif // CheckNetworkWorks_
                    {
                        yarp::os::Network     yarp; // This is necessary to establish any
                                                    // connections to the YARP infrastructure
                        yarp::os::ConstString channelNameRequest(MpM_REQREP_DICT_NAME_KEY ":");
                        
                        Initialize(*argv);
                        TunnelClient * stuff = new TunnelClient;
                        
                        if (stuff)
                        {
#if defined(MpM_ReportOnConnections)
                            stuff->setReporter(reporter, true);
#endif // defined(MpM_ReportOnConnections)
                            channelNameRequest += namePattern;
                            if (stuff->findService(channelNameRequest.c_str()))
                            {
                                SOCKET listenSocket = setUpListeningPost(listenPort);

                                if (INVALID_SOCKET != listenSocket)
                                {
                                    OD_LOG("(INVALID_SOCKET != listenSocket)"); //####
                                    if (stuff->connectToService())
                                    {
                                        yarp::os::ConstString serviceAddress;
                                        int                   servicePort;
                                        
                                        if (stuff->getAddress(serviceAddress, servicePort))
                                        {
                                            handleConnections(listenSocket, serviceAddress,
                                                              servicePort);
                                        }
                                        else
                                        {
                                            OD_LOG("! (stuff->getAddress(serviceAddress, " //####
                                                   "servicePort))"); //####
#if MAC_OR_LINUX_
                                            GetLogger().fail("Problem fetching the address "
                                                             "information.");
#else // ! MAC_OR_LINUX_
                                            cerr << "Problem fetching the address information." <<
                                                    endl;
#endif // ! MAC_OR_LINUX_
                                        }
                                    }
                                    else
                                    {
                                        OD_LOG("! (stuff->connectToService())"); //####
#if MAC_OR_LINUX_
                                        GetLogger().fail("Could not connect to the required "
                                                         "service.");
#else // ! MAC_OR_LINUX_
                                        cerr << "Could not connect to the required service." <<
                                                endl;
#endif // ! MAC_OR_LINUX_
                                    }
#if MAC_OR_LINUX_
                                    shutdown(listenSocket, SHUT_RDWR);
                                    close(listenSocket);
#else // ! MAC_OR_LINUX_
                                    shutdown(listenSocket, SD_BOTH);
                                    closesocket(listenSocket);
#endif // ! MAC_OR_LINUX_
                                }
                            }
                            else
                            {
                                OD_LOG("! (stuff->findService(channelNameRequest)"); //####
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
                catch (...)
                {
                    OD_LOG("Exception caught"); //####
                }
            }
            else
            {
#if MAC_OR_LINUX_
                GetLogger().fail("Invalid argument(s).");
#else // ! MAC_OR_LINUX_
                cerr << "Invalid argument(s)." << endl;
#endif // ! MAC_OR_LINUX_
            }
            yarp::os::Network::fini();
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
