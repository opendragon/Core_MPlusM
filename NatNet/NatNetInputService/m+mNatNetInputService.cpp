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

#include "m+mNatNetInputService.hpp"
#include "m+mNatNetInputRequests.hpp"
#include "m+mNatNetInputThread.hpp"

#include <m+m/m+mEndpoint.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

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
    ODL_ENTER(); //####
    ODL_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    ODL_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
            serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    ODL_I1("argc = ", argc); //####
    ODL_EXIT_P(this); //####
} // NatNetInputService::NatNetInputService

NatNetInputService::~NatNetInputService(void)
{
    ODL_OBJENTER(); //####
    stopStreams();
    ODL_OBJEXIT(); //####
} // NatNetInputService::~NatNetInputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool
NatNetInputService::configure(const yarp::os::Bottle & details)
{
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
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
                    ODL_S1s("_hostName <- ", _hostName); //####
                    _commandPort = secondNumber;
                    ODL_I1("_commandPort <- ", _commandPort); //####
                    _dataPort = thirdNumber;
                    ODL_I1("_dataPort <- ", _dataPort); //####
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
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // NatNetInputService::configure

bool
NatNetInputService::getConfiguration(yarp::os::Bottle & details)
{
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    details.addString(_hostName);
    details.addInt(_commandPort);
    details.addInt(_dataPort);
    ODL_OBJEXIT_B(result); //####
    return result;
} // NatNetInputService::getConfiguration

bool
NatNetInputService::setUpStreamDescriptions(void)
{
    ODL_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");

    _outDescriptions.clear();
    description._portName = rootName + "output";
    description._portProtocol = "NN";
    description._protocolDescription = "A list of dictionaries with position values";
    _outDescriptions.push_back(description);
    ODL_OBJEXIT_B(result); //####
    return result;
} // NatNetInputService::setUpStreamDescriptions

bool
NatNetInputService::shutDownOutputStreams(void)
{
    ODL_OBJENTER(); //####
    bool result = inherited::shutDownOutputStreams();

    if (_eventThread)
    {
        _eventThread->clearOutputChannel();
    }
    ODL_EXIT_B(result); //####
    return result;
} // NatNetInputService::shutDownOutputStreams

void
NatNetInputService::startStreams(void)
{
    ODL_OBJENTER(); //####
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
                ODL_LOG("! (_eventThread->start())"); //####
                cerr << "Could not start auxiliary thread." << endl;
                delete _eventThread;
                _eventThread = NULL;
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // NatNetInputService::startStreams

void
NatNetInputService::stopStreams(void)
{
    ODL_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            if (_eventThread)
            {

                _eventThread->stop();
                for ( ; _eventThread->isRunning(); )
                {
                    ConsumeSomeTime(IO_SERVICE_DELAY_FACTOR_);
                }
                delete _eventThread;
                _eventThread = NULL;
            }
            clearActive();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // NatNetInputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
