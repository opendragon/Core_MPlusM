//--------------------------------------------------------------------------------------------------
//
//  File:       m+mBlobOutputService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Blob output service.
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
//  Created:    2015-06-23
//
//--------------------------------------------------------------------------------------------------

#include "m+mBlobOutputService.h"

#include "m+mBlobOutputInputHandler.h"
#include "m+mBlobOutputRequests.h"

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
 @brief The class definition for the %Blob output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Blob;
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

BlobOutputService::BlobOutputService(const YarpString & launchPath,
                                     const int          argc,
                                     char * *           argv,
                                     const YarpString & tag,
                                     const YarpString & serviceEndpointName,
                                     const YarpString & servicePortNumber) :
    inherited(launchPath, argc, argv, tag, true, MpM_BLOBOUTPUT_CANONICAL_NAME_,
              BLOBOUTPUT_SERVICE_DESCRIPTION_, "", serviceEndpointName, servicePortNumber),
	_outPort(9876), _networkSocket(INVALID_SOCKET), _inHandler(new BlobOutputInputHandler(*this))
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_P1("argv = ", argv); //####
    OD_LOG_EXIT_P(this); //####
} // BlobOutputService::BlobOutputService

BlobOutputService::~BlobOutputService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    delete _inHandler;
    OD_LOG_OBJEXIT(); //####
} // BlobOutputService::~BlobOutputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool BlobOutputService::configure(const yarp::os::Bottle & details)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = false;
    
    try
    {
        if (! isActive())
        {
            if (1 == details.size())
            {
                yarp::os::Value firstValue(details.get(0));
                
				if (firstValue.isInt())
				{
                    std::stringstream buff;

					_outPort = firstValue.asInt();
                    buff << "Output port is " << _outPort;
                    setExtraInformation(buff.str());
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
} // BlobOutputService::configure

void BlobOutputService::deactivateConnection(void)
{
    OD_LOG_ENTER(); //####
    clearActive();
	cerr << "connection is dead" << endl; //!!!!
    if (_inHandler)
    {
        _inHandler->setSocket(INVALID_SOCKET);
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
    OD_LOG_EXIT(); //####
} // BlobOutputService::deactivateConnection

void BlobOutputService::restartStreams(void)
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
} // BlobOutputService::restartStreams

bool BlobOutputService::setUpStreamDescriptions(void)
{
    OD_LOG_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");
    
    _inDescriptions.clear();
    description._portName = rootName + "input";
    description._portProtocol = "b";
    description._protocolDescription = "A binary blob";
    _inDescriptions.push_back(description);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // BlobOutputService::setUpStreamDescriptions

bool BlobOutputService::start(void)
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
} // BlobOutputService::start

void BlobOutputService::startStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
			if (_inHandler)
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
					}
					else
                    {
                        listen(listenSocket, SOMAXCONN);
                        _networkSocket = accept(listenSocket, 0, 0);
                        if (INVALID_SOCKET == _networkSocket)
						{
						}
						else
                        {
                            _inHandler->setSocket(_networkSocket);
                            getInletStream(0)->setReader(*_inHandler);
                            setActive();
                        }
                    }
                    close(listenSocket);
                }
#else // ! MAC_OR_LINUX_
                if (WSAStartup(wVersionRequested, &ww))
				{
					cerr << "could not start up WSA" << endl; //!!!!
				}
				else
                {
                    if ((2 == LOBYTE(ww.wVersion)) && (2 == HIBYTE(ww.wVersion)))
                    {
						cerr << "creating socket" << endl; //!!!!
                        SOCKET listenSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
                        
                        if (INVALID_SOCKET == listenSocket)
						{
							cerr << "problem creating socket" << endl; //!!!!
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
								cerr << "problem binding to socket" << endl; //!!!!
							}
							else
                            {
								cerr << "listening for connection" << endl; //!!!!
                                listen(listenSocket, SOMAXCONN);
								cerr << "accepting the connection" << endl; //!!!!
                                _networkSocket = accept(listenSocket, 0, 0);
                                if (INVALID_SOCKET == _networkSocket)
								{
									cerr << "problem accepting a connection" << endl; //!!!!
								}
								else
                                {
									cerr << "connection is live" << endl; //!!!!
                                    _inHandler->setSocket(_networkSocket);
                                    getInletStream(0)->setReader(*_inHandler);
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
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // BlobOutputService::startStreams

bool BlobOutputService::stop(void)
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
} // BlobOutputService::stop

void BlobOutputService::stopStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            deactivateConnection();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // BlobOutputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
