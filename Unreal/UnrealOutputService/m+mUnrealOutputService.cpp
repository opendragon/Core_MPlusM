//--------------------------------------------------------------------------------------------------
//
//  File:       m+mUnrealOutputService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Unreal output service.
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
//  Created:    2014-11-18
//
//--------------------------------------------------------------------------------------------------

#include "m+mUnrealOutputService.h"

#include "m+mUnrealOutputLeapInputHandler.h"
#include "m+mUnrealOutputRequests.h"
#include "m+mUnrealOutputViconInputHandler.h"

#include <m+m/m+mEndpoint.h>
#include <m+m/m+mGeneralChannel.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the %Unreal output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Unreal;
using std::cerr;
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

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

UnrealOutputService::UnrealOutputService(const Utilities::DescriptorVector & argumentList,
                                         const YarpString &                  launchPath,
                                         const int                           argc,
                                         char * *                            argv,
                                         const YarpString &                  tag,
                                         const YarpString &                  serviceEndpointName,
                                         const YarpString &                  servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true, MpM_UNREALOUTPUT_CANONICAL_NAME_,
              UNREALOUTPUT_SERVICE_DESCRIPTION_, "", serviceEndpointName, servicePortNumber),
    _translationScale(1.0), _outPort(9876), _networkSocket(INVALID_SOCKET),
    _inLeapHandler(new UnrealOutputLeapInputHandler(*this)),
    _inViconHandler(new UnrealOutputViconInputHandler(*this))
{
    ODL_ENTER(); //####
    ODL_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    ODL_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    ODL_LL1("argc = ", argc); //####
    ODL_EXIT_P(this); //####
} // UnrealOutputService::UnrealOutputService

UnrealOutputService::~UnrealOutputService(void)
{
    ODL_OBJENTER(); //####
    stopStreams();
    delete _inLeapHandler;
    delete _inViconHandler;
    ODL_OBJEXIT(); //####
} // UnrealOutputService::~UnrealOutputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

DEFINE_CONFIGURE_(UnrealOutputService)
{
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = false;
    
    try
    {
        if (2 <= details.size())
        {
            yarp::os::Value firstValue(details.get(0));
            yarp::os::Value secondValue(details.get(1));
            
            if (firstValue.isInt() && secondValue.isDouble())
            {
                std::stringstream buff;
                
                _outPort = firstValue.asInt();
                ODL_LL1("_outPort <- ", _outPort); //####
                _translationScale = secondValue.asDouble();
                ODL_D1("_translationScale <- ", _translationScale); //####
                buff << "Output port is " << _outPort << ", translation scale is " <<
                        _translationScale;
                setExtraInformation(buff.str());
                result = true;
            }
            else
            {
                cerr << "One or more inputs have the wrong type." << endl;
            }
        }
        else
        {
            cerr << "Missing input(s)." << endl;
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // UnrealOutputService::configure

void
UnrealOutputService::deactivateConnection(void)
{
    ODL_ENTER(); //####
    clearActive();
    cerr << "connection is dead" << endl; //!!!!
    if (_inLeapHandler)
    {
        _inLeapHandler->setSocket(INVALID_SOCKET);
    }
    if (_inViconHandler)
    {
        _inViconHandler->setSocket(INVALID_SOCKET);
    }
    if (INVALID_SOCKET != _networkSocket)
    {
#if MAC_OR_LINUX_
        shutdown(_networkSocket, SHUT_RDWR);
        close(_networkSocket);
#else // ! MAC_OR_LINUX_
        shutdown(_networkSocket, SD_BOTH);
        closesocket(_networkSocket);
#endif // ! MAC_OR_LINUX_
        _networkSocket = INVALID_SOCKET;
    }
    ODL_EXIT(); //####
} // UnrealOutputService::deactivateConnection

DEFINE_DISABLEMETRICS_(UnrealOutputService)
{
    ODL_OBJENTER(); //####
    inherited::disableMetrics();
    if (_inLeapHandler)
    {
        _inLeapHandler->disableMetrics();
    }
    if (_inViconHandler)
    {
        _inViconHandler->disableMetrics();
    }
    ODL_OBJEXIT(); //####
} // UnrealOutputService::disableMetrics

DEFINE_ENABLEMETRICS_(UnrealOutputService)
{
    ODL_OBJENTER(); //####
    inherited::enableMetrics();
    if (_inLeapHandler)
    {
        _inLeapHandler->enableMetrics();
    }
    if (_inViconHandler)
    {
        _inViconHandler->enableMetrics();
    }
    ODL_OBJEXIT(); //####
} // UnrealOutputService::enableMetrics

DEFINE_GETCONFIGURATION_(UnrealOutputService)
{
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    details.addInt(_outPort);
    details.addDouble(_translationScale);
    ODL_OBJEXIT_B(result); //####
    return result;
} // UnrealOutputService::getConfiguration

DEFINE_RESTARTSTREAMS_(UnrealOutputService)
{
    ODL_OBJENTER(); //####
    try
    {
        // No special processing needed.
        stopStreams();
        startStreams();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // UnrealOutputService::restartStreams

DEFINE_SETUPSTREAMDESCRIPTIONS_(UnrealOutputService)
{
    ODL_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");
    
    _inDescriptions.clear();
    description._portName = rootName + "leapinput";
    description._portProtocol = "LEAP";
    description._protocolDescription = "A list of hands followed by a list of tools\n"
                        "Each hand being a dictionary with an arm and a list of fingers\n"
                        "Each finger being a dictionary with a list of bones";
    _inDescriptions.push_back(description);
    description._portName = rootName + "viconinput";
    description._portProtocol = "VICONDS";
    description._protocolDescription = "A list of subjects\n"
                "Each subject being a list of the subject name and a dictionary of segments\n"
                "Each segment being a dictionary with name, translation and rotation";
    _inDescriptions.push_back(description);
    ODL_OBJEXIT_B(result); //####
    return result;
} // UnrealOutputService::setUpStreamDescriptions

DEFINE_STARTSERVICE_(UnrealOutputService)
{
    ODL_OBJENTER(); //####
    try
    {
        if (! isStarted())
        {
            inherited::startService();
            if (isStarted())
            {
            
            }
            else
            {
                ODL_LOG("! (isStarted())"); //####
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(isStarted()); //####
    return isStarted();
} // UnrealOutputService::startService

DEFINE_STARTSTREAMS_(UnrealOutputService)
{
    ODL_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_inLeapHandler && _inViconHandler)
            {
#if MAC_OR_LINUX_
                SOCKET listenSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
#else // ! MAC_OR_LINUX_
                WORD    wVersionRequested = MAKEWORD(2, 2);
                WSADATA ww;
#endif // ! MAC_OR_LINUX_
                
#if MAC_OR_LINUX_
                if (INVALID_SOCKET == listenSocket)
                {
                    cerr << "Could not create socket." << endl;
                }
                else
                {
                    struct sockaddr_in addr;
                    
                    memset(&addr, 0, sizeof(addr));
                    addr.sin_family = AF_INET;
                    addr.sin_port = htons(_outPort);
                    addr.sin_addr.s_addr = htonl(INADDR_ANY);
                    if (bind(listenSocket, reinterpret_cast<struct sockaddr *>(&addr),
                             sizeof(addr)))
                    {
                        cerr << "Could not bind to socket." << endl;
                    }
                    else
                    {
                        listen(listenSocket, SOMAXCONN);
                        _networkSocket = accept(listenSocket, 0, 0);
                        if (INVALID_SOCKET == _networkSocket)
                        {
                            cerr << "Could not accept connection." << endl;
                        }
                        else
                        {
                            _inLeapHandler->setSocket(_networkSocket);
                            _inLeapHandler->setScale(_translationScale);
                            _inLeapHandler->setChannel(getInletStream(0));
                            getInletStream(0)->setReader(*_inLeapHandler);
                            _inViconHandler->setSocket(_networkSocket);
                            _inViconHandler->setScale(_translationScale);
                            _inViconHandler->setChannel(getInletStream(1));
                            getInletStream(1)->setReader(*_inViconHandler);
                            setActive();
                        }
                    }
                    close(listenSocket);
                }
#else // ! MAC_OR_LINUX_
                if (WSAStartup(wVersionRequested, &ww))
                {
                    cerr << "Could not start up WSA" << endl;
                }
                else
                {
                    if ((2 == LOBYTE(ww.wVersion)) && (2 == HIBYTE(ww.wVersion)))
                    {
                        cerr << "creating socket" << endl; //!!!!
                        SOCKET listenSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
                        
                        if (INVALID_SOCKET == listenSocket)
                        {
                            cerr << "Could not create socket." << endl;
                        }
                        else
                        {
                            SOCKADDR_IN addr;
                            
                            addr.sin_family = AF_INET;
                            addr.sin_port = htons(_outPort);
                            addr.sin_addr.s_addr = htonl(INADDR_ANY);
                            cerr << "binding to port" << endl; //!!!!
                            if (SOCKET_ERROR == bind(listenSocket,
                                                     reinterpret_cast<LPSOCKADDR>(&addr),
                                                     sizeof(addr)))
                            {
                                cerr << "Could not bind to socket." << endl;
                            }
                            else
                            {
                                cerr << "listening for connection" << endl; //!!!!
                                listen(listenSocket, SOMAXCONN);
                                cerr << "accepting the connection" << endl; //!!!!
                                _networkSocket = accept(listenSocket, 0, 0);
                                if (INVALID_SOCKET == _networkSocket)
                                {
                                    cerr << "Could not accept connection." << endl;
                                }
                                else
                                {
                                    cerr << "connection is live" << endl; //!!!!
                                    _inLeapHandler->setSocket(_networkSocket);
                                    _inLeapHandler->setScale(_translationScale);
                                    _inLeapHandler->setChannel(getInletStream(0));
                                    getInletStream(0)->setReader(*_inLeapHandler);
                                    _inViconHandler->setSocket(_networkSocket);
                                    _inViconHandler->setScale(_translationScale);
                                    _inViconHandler->setChannel(getInletStream(1));
                                    getInletStream(1)->setReader(*_inViconHandler);
                                    setActive();
                                }
                            }
                            shutdown(listenSocket, SD_BOTH);
                            closesocket(listenSocket);
                        }
                    }
                    else
                    {
                        cerr << "WSA version not available" << endl;
                        WSACleanup();
                    }
                }
#endif // ! MAC_OR_LINUX_
            }
            else if (INVALID_SOCKET != _networkSocket)
            {
#if MAC_OR_LINUX_
                shutdown(_networkSocket, SHUT_RDWR);
                close(_networkSocket);
#else // ! MAC_OR_LINUX_
                shutdown(_networkSocket, SD_BOTH);
                closesocket(_networkSocket);
#endif // ! MAC_OR_LINUX_
                _networkSocket = INVALID_SOCKET;
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // UnrealOutputService::startStreams

DEFINE_STOPSERVICE_(UnrealOutputService)
{
    ODL_OBJENTER(); //####
    bool result;
    
    try
    {
        result = inherited::stopService();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // UnrealOutputService::stopService

DEFINE_STOPSTREAMS_(UnrealOutputService)
{
    ODL_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            deactivateConnection();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // UnrealOutputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
