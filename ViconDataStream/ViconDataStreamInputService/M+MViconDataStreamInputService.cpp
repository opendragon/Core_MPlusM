//--------------------------------------------------------------------------------------------------
//
//  File:       M+MViconDataStreamInputService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the Vicon DataStream input service.
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
//  Created:    2014-11-07
//
//--------------------------------------------------------------------------------------------------

#include "M+MViconDataStreamInputService.h"
#include "M+MViconDataStreamEventThread.h"
#include "M+MViconDataStreamInputRequests.h"

#include <mpm/M+MEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the Vicon DataStream input service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::ViconDataStream;

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

ViconDataStreamInputService::ViconDataStreamInputService(const yarp::os::ConstString & launchPath,
                                                         const yarp::os::ConstString & tag,
                                                         const yarp::os::ConstString &
                                                                                serviceEndpointName,
                                                         const yarp::os::ConstString &
                                                                                servicePortNumber) :
    inherited(launchPath, tag, true, MpM_VICONDATASTREAMINPUT_CANONICAL_NAME,
              "The Vicon DataStream input service", "", serviceEndpointName, servicePortNumber),
    _eventThread(NULL), _hostName("localhost"), _hostPort(801)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_EXIT_P(this); //####
} // ViconDataStreamInputService::ViconDataStreamInputService

ViconDataStreamInputService::~ViconDataStreamInputService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    OD_LOG_OBJEXIT(); //####
} // ViconDataStreamInputService::~ViconDataStreamInputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool ViconDataStreamInputService::configure(const yarp::os::Bottle & details)
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
                
                if (firstValue.isString() && secondValue.isInt())
                {
                    int secondNumber = secondValue.asInt();
                    
                    if (0 < secondNumber)
                    {
						_hostName = firstValue.asString();
						OD_LOG_S1s("_hostName <- ", _hostName); //####
                        _hostPort = secondNumber;
                        result = true;
                    }
                }
            }
        }
        result = true;
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // ViconDataStreamInputService::configure

void ViconDataStreamInputService::restartStreams(void)
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
} // ViconDataStreamInputService::restartStreams

bool ViconDataStreamInputService::setUpStreamDescriptions(void)
{
    OD_LOG_OBJENTER(); //####
    bool                  result = true;
    ChannelDescription    description;
    yarp::os::ConstString rootName(getEndpoint().getName() + "/");
    
    _outDescriptions.clear();
    description._portName = rootName + "output";
    description._portProtocol = "VICONDS";
    description._protocolDescription = "A list of subjects\n"
                    "Each subject being a list of the subject name and a dictionary of segments\n"
                    "Each segment being a dictionary with name, translation and rotation";
    _outDescriptions.push_back(description);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // ViconDataStreamInputService::setUpStreamDescriptions

bool ViconDataStreamInputService::shutDownOutputStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = inherited::shutDownOutputStreams();
    
    if (_eventThread)
    {
        _eventThread->clearOutputChannel();
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // ViconDataStreamInputService::shutDownOutputStreams

bool ViconDataStreamInputService::start(void)
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
} // ViconDataStreamInputService::start

void ViconDataStreamInputService::startStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
		    std::stringstream nameAndPort;

			nameAndPort << _hostName.c_str() << ":" << _hostPort;
            _eventThread = new ViconDataStreamEventThread(_outStreams.at(0), nameAndPort.str());
            _eventThread->start();
            setActive();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // ViconDataStreamInputService::startStreams

bool ViconDataStreamInputService::stop(void)
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
} // ViconDataStreamInputService::stop

void ViconDataStreamInputService::stopStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            _eventThread->stop();
            for ( ; _eventThread->isRunning(); )
            {
                yarp::os::Time::delay(ONE_SECOND_DELAY / 3.9);
            }
            delete _eventThread;
            _eventThread = NULL;
            clearActive();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // ViconDataStreamInputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
