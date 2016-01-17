//--------------------------------------------------------------------------------------------------
//
//  File:       m+mNatNetInputService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Natural Point NatNet input service.
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
//  Created:    2015-04-13
//
//--------------------------------------------------------------------------------------------------

#include "m+mNatNetInputService.h"

#include "m+mNatNetInputRequests.h"
#include "m+mNatNetInputThread.h"

#include <m+m/m+mEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the Natural Point %NatNet input service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::NatNet;
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

NatNetInputService::NatNetInputService(const Utilities::DescriptorVector & argumentList,
                                       const YarpString &                  launchPath,
                                       const int                           argc,
                                       char * *                            argv,
                                       const YarpString &                  tag,
                                       const YarpString &                  serviceEndpointName,
                                       const YarpString &                  servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true, MpM_NATNETINPUT_CANONICAL_NAME_,
              NATNETINPUT_SERVICE_DESCRIPTION_, "", serviceEndpointName, servicePortNumber),
    _commandPort(NATNETINPUT_DEFAULT_COMMAND_PORT_), _dataPort(NATNETINPUT_DEFAULT_DATA_PORT_),
    _eventThread(NULL)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_EXIT_P(this); //####
} // NatNetInputService::NatNetInputService

NatNetInputService::~NatNetInputService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    OD_LOG_OBJEXIT(); //####
} // NatNetInputService::~NatNetInputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

DEFINE_CONFIGURE_(NatNetInputService)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = false;
    
    try
    {
        if (3 <= details.size())
        {
            yarp::os::Value firstValue(details.get(0));
            yarp::os::Value secondValue(details.get(1));
            yarp::os::Value thirdValue(details.get(2));

            if (firstValue.isString() && secondValue.isInt() && thirdValue.isInt())
            {
                int secondNumber = secondValue.asInt();
                int thirdNumber = thirdValue.asInt();

                if ((0 < secondNumber) && (0 < thirdNumber))
                {
                    std::stringstream buff;

                    _hostName = firstValue.asString();
                    OD_LOG_S1s("_hostName <- ", _hostName); //####
                    _commandPort = secondNumber;
                    OD_LOG_LL1("_commandPort <- ", _commandPort); //####
                    _dataPort = thirdNumber;
                    OD_LOG_LL1("_dataPort <- ", _dataPort); //####
                    buff << "Host name is '" << _hostName.c_str() << "', command port is " <<
                            _commandPort << ", data port is " << _dataPort;
                    setExtraInformation(buff.str());
                    result = true;
                }
                else
                {
                    cerr << "One or more inputs are out of range." << endl;
                }
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
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // NatNetInputService::configure

DEFINE_GETCONFIGURATION_(NatNetInputService)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    details.addString(_hostName);
    details.addInt(_commandPort);
    details.addInt(_dataPort);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // NatNetInputService::getConfiguration

DEFINE_RESTARTSTREAMS_(NatNetInputService)
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
} // NatNetInputService::restartStreams

DEFINE_SETUPSTREAMDESCRIPTIONS_(NatNetInputService)
{
    OD_LOG_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");
    
    _outDescriptions.clear();
    description._portName = rootName + "output";
    description._portProtocol = "NN";
    description._protocolDescription = "A list of dictionaries with position values";
    _outDescriptions.push_back(description);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // NatNetInputService::setUpStreamDescriptions

DEFINE_SHUTDOWNOUTPUTSTREAMS_(NatNetInputService)
{
    OD_LOG_OBJENTER(); //####
    bool result = inherited::shutDownOutputStreams();
    
    if (_eventThread)
    {
        _eventThread->clearOutputChannel();
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // NatNetInputService::shutDownOutputStreams

DEFINE_STARTSERVICE_(NatNetInputService)
{
    OD_LOG_OBJENTER(); //####
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
} // NatNetInputService::startService

DEFINE_STARTSTREAMS_(NatNetInputService)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            _eventThread = new NatNetInputThread(getOutletStream(0), _hostName, _commandPort,
                                                 _dataPort);
            if (_eventThread->start())
            {
                setActive();
            }
            else
            {
                OD_LOG("! (_eventThread->start())"); //####
                cerr << "Could not start auxiliary thread." << endl;
                delete _eventThread;
                _eventThread = NULL;
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // NatNetInputService::startStreams

DEFINE_STOPSERVICE_(NatNetInputService)
{
    OD_LOG_OBJENTER(); //####
    bool result;
    
    try
    {
        result = inherited::stopService();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // NatNetInputService::stopService

DEFINE_STOPSTREAMS_(NatNetInputService)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            if (_eventThread)
            {

                _eventThread->stop();
                for ( ; _eventThread->isRunning(); )
                {
                    yarp::os::Time::delay(ONE_SECOND_DELAY_ / 3.9);
                }
                delete _eventThread;
                _eventThread = NULL;
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
} // NatNetInputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
