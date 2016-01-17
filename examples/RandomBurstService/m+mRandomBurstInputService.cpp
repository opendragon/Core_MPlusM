//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRandomBurstInputService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Random Burst input service.
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
//  Created:    2014-06-24
//
//--------------------------------------------------------------------------------------------------

#include "m+mRandomBurstInputService.h"

#include "m+mRandomBurstInputRequests.h"
#include "m+mRandomBurstInputThread.h"

#include <m+m/m+mEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the Random Burst input service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;
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

RandomBurstInputService::RandomBurstInputService(const Utilities::DescriptorVector & argumentList,
                                                 const YarpString &                  launchPath,
                                                 const int                           argc,
                                                 char * *                            argv,
                                                 const YarpString &                  tag,
                                                 const YarpString &
                                                                             serviceEndpointName,
                                                 const YarpString &
                                                                             servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true, MpM_RANDOMBURSTINPUT_CANONICAL_NAME_,
              RANDOMBURSTINPUT_SERVICE_DESCRIPTION_, "", serviceEndpointName, servicePortNumber),
    _generator(NULL), _burstPeriod(1), _burstSize(1)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_EXIT_P(this); //####
} // RandomBurstInputService::RandomBurstInputService

RandomBurstInputService::~RandomBurstInputService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    OD_LOG_OBJEXIT(); //####
} // RandomBurstInputService::~RandomBurstInputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

DEFINE_CONFIGURE_(RandomBurstInputService)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = false;
    
    try
    {
        if (2 <= details.size())
        {
            yarp::os::Value firstValue(details.get(0));
            yarp::os::Value secondValue(details.get(1));
            
            if (firstValue.isDouble() && secondValue.isInt())
            {
                double firstNumber = firstValue.asDouble();
                int    secondNumber = secondValue.asInt();
                
                if ((0 < firstNumber) && (0 < secondNumber))
                {
                    std::stringstream buff;
                    
                    _burstPeriod = firstNumber;
                    _burstSize = secondNumber;
                    OD_LOG_D1("_burstPeriod <- ", _burstPeriod); //####
                    OD_LOG_LL1("_burstSize <- ", _burstSize); //####
                    buff << "Burst period is " << _burstPeriod << ", burst size is " << _burstSize;
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
} // RandomBurstInputService::configure

DEFINE_GETCONFIGURATION_(RandomBurstInputService)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    details.addDouble(_burstPeriod);
    details.addInt(_burstSize);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // RandomBurstInputService::getConfiguration

DEFINE_RESTARTSTREAMS_(RandomBurstInputService)
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
} // RandomBurstInputService::restartStreams

DEFINE_SETUPSTREAMDESCRIPTIONS_(RandomBurstInputService)
{
    OD_LOG_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");
    
    _outDescriptions.clear();
    description._portName = rootName + "output";
    description._portProtocol = "d+";
    description._protocolDescription = "One or more numeric values";
    _outDescriptions.push_back(description);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // RandomBurstInputService::setUpStreamDescriptions

DEFINE_SHUTDOWNOUTPUTSTREAMS_(RandomBurstInputService)
{
    OD_LOG_OBJENTER(); //####
    bool result = inherited::shutDownOutputStreams();
    
    if (_generator)
    {
        _generator->clearOutputChannel();
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // RandomBurstInputService::shutDownOutputStreams

DEFINE_STARTSERVICE_(RandomBurstInputService)
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
} // RandomBurstInputService::startService

DEFINE_STARTSTREAMS_(RandomBurstInputService)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            _generator = new RandomBurstInputThread(getOutletStream(0), _burstPeriod, _burstSize);
            if (_generator->start())
            {
                setActive();
            }
            else
            {
                OD_LOG("! (generator->start())"); //####
                cerr << "Could not start auxiliary thread." << endl;
                delete _generator;
                _generator = NULL;
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // RandomBurstInputService::startStreams

DEFINE_STOPSERVICE_(RandomBurstInputService)
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
} // RandomBurstInputService::stopService

DEFINE_STOPSTREAMS_(RandomBurstInputService)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            if (_generator)
            {
                _generator->stop();
                for ( ; _generator->isRunning(); )
                {
                    yarp::os::Time::delay(_burstSize / 3.9);
                }
                delete _generator;
                _generator = NULL;
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
} // RandomBurstInputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
