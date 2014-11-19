//--------------------------------------------------------------------------------------------------
//
//  File:       M+MUnrealOutputService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the Unreal output service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and/or
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
//  Created:    2014-11-18
//
//--------------------------------------------------------------------------------------------------

#include "M+MUnrealOutputService.h"
#include "M+MUnrealOutputInputHandler.h"
#include "M+MUnrealOutputRequests.h"

#include <mpm/M+MEndpoint.h>
#include <mpm/M+MGeneralChannel.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if (! MAC_OR_LINUX_)
# pragma comment(lib, "ws2_32.lib")
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the Unreal output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Unreal;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

UnrealOutputService::UnrealOutputService(const yarp::os::ConstString & launchPath,
                                         const yarp::os::ConstString & tag,
                                         const yarp::os::ConstString & serviceEndpointName,
                                         const yarp::os::ConstString & servicePortNumber) :
    inherited(launchPath, tag, true, MpM_UNREALOUTPUT_CANONICAL_NAME,
              "The Unreal output service", "", serviceEndpointName, servicePortNumber),
	_translationScale(1.0), _outPort(9876), _networkSocket(INVALID_SOCKET),
    _inHandler(new UnrealOutputInputHandler)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_EXIT_P(this); //####
} // UnrealOutputService::UnrealOutputService

UnrealOutputService::~UnrealOutputService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    delete _inHandler;
    OD_LOG_OBJEXIT(); //####
} // UnrealOutputService::~UnrealOutputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool UnrealOutputService::configure(const yarp::os::Bottle & details)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = false;
    
    try
    {
        if (! isActive())
        {
            if (2 == details.size())
            {
                yarp::os::Value firstValue(details.get(0));
                yarp::os::Value secondValue(details.get(1));
                
				if (firstValue.isInt() && secondValue.isDouble())
				{
					_outPort = firstValue.asInt();
                    _translationScale = secondValue.asDouble();
					result = true;
				}
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // UnrealOutputService::configure

void UnrealOutputService::restartStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        // No special processing needed.
        stopStreams();
        startStreams();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // UnrealOutputService::restartStreams

bool UnrealOutputService::setUpStreamDescriptions(void)
{
    OD_LOG_OBJENTER(); //####
    bool                  result = true;
    ChannelDescription    description;
    yarp::os::ConstString rootName(getEndpoint().getName() + "/");
    
    _inDescriptions.clear();
    description._portName = rootName + "input";
    description._portProtocol = "VICONDS";
    description._protocolDescription = "Vicon DataStream values";
    _inDescriptions.push_back(description);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // UnrealOutputService::setUpStreamDescriptions

bool UnrealOutputService::start(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isStarted())
        {
            inherited::start();
            if (isStarted())
            {
            
            }
            else
            {
                OD_LOG("! (isStarted())"); //####
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(isStarted()); //####
    return isStarted();
} // UnrealOutputService::start

void UnrealOutputService::startStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
			if (_inHandler)
			{
#if MAC_OR_LINUX_
                SOCKET listenSocket = socket(AF_INET, SOCK_SEQPACKET, IPPROTO_TCP);
                
                if (INVALID_SOCKET != listenSocket)
                {
                    struct sockaddr_in addr;
                    
                    addr.sin_family = AF_INET;
                    addr.sin_port = htons(_outPort);
                    addr.sin_addr.s_addr = htonl(INADDR_ANY);
                    if (! bind(listenSocket, reinterpret_cast<struct sockaddr *>(&addr),
                               sizeof(addr)))
                    {
                        listen(listenSocket, SOMAXCONN);
                        _networkSocket = accept(listenSocket, 0, 0);
                        if (INVALID_SOCKET != _networkSocket)
                        {
                            _inHandler->setSocket(_networkSocket);
                            _inHandler->setScale(_translationScale);
                            _inStreams.at(0)->setReader(*_inHandler);
                            setActive();
                        }
                    }
                    close(listenSocket);
                }
#else // ! MAC_OR_LINUX_
                WORD    wVersionRequested = MAKEWORD(2, 2);
                WSADATA ww;
                
                if (! WSAStartup(wVersionRequested, &ww))
                {
                    if ((2 == LOBYTE(ww.wVersion)) && (2 == HIBYTE(ww.wVersion)))
                    {
                        SOCKET listenSocket = socket(AF_INET, SOCK_SEQPACKET, IPPROTO_TCP);
                        
                        if (INVALID_SOCKET != listenSocket)
                        {
                            SOCKADDR_IN addr;
                            
                            addr.sin_family = AF_INET;
                            addr.sin_port = htons(_outPort);
                            addr.sin_addr.s_addr = htonl(INADDR_ANY);
                            if (SOCKET_ERROR != bind(listenSocket,
                                                     reinterpret_cast<LPSOCKADDR>(&addr),
                                                     sizeof(addr)))
                            {
                                listen(listenSocket, SOMAXCONN);
                                _networkSocket = accept(listenSocket, 0, 0);
                                if (INVALID_SOCKET != _networkSocket)
                                {
                                    _inHandler->setSocket(_networkSocket);
                                    _inHandler->setScale(_translationScale);
                                    _inStreams.at(0)->setReader(*_inHandler);
                                    setActive();
                                }
                            }
                            closesocket(listenSocket);
                        }
                    }
                    else
                    {
                        WSACleanup();
                    }
                }
#endif // ! MAC_OR_LINUX_
			}
			else
			{
				if (INVALID_SOCKET != _networkSocket)
				{
#if MAC_OR_LINUX_
                    close(_networkSocket);
#else // ! MAC_OR_LINUX_
                    closesocket(_networkSocket);
#endif // ! MAC_OR_LINUX_
					_networkSocket = INVALID_SOCKET;
				}
			}
		}
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // UnrealOutputService::startStreams

bool UnrealOutputService::stop(void)
{
    OD_LOG_OBJENTER(); //####
    bool result;
    
    try
    {
        result = inherited::stop();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // UnrealOutputService::stop

void UnrealOutputService::stopStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            if (_inHandler)
            {
				_inHandler->setSocket(INVALID_SOCKET);
            }
			if (INVALID_SOCKET != _networkSocket)
			{
#if MAC_OR_LINUX_
                close(_networkSocket);
#else // ! MAC_OR_LINUX_
                closesocket(_networkSocket);
#endif // ! MAC_OR_LINUX_
                _networkSocket = INVALID_SOCKET;
			}
            clearActive();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // UnrealOutputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)