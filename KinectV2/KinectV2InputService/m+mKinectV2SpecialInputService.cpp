//--------------------------------------------------------------------------------------------------
//
//  File:       m+mKinectV2SpecialInputService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Kinect V2 special input service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by Simon Fraser University.
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
//  Created:    2015-10-30
//
//--------------------------------------------------------------------------------------------------

#include "m+mKinectV2SpecialInputService.h"

#include "m+mKinectV2SpecialEventThread.h"
#include "m+mKinectV2SpecialInputRequests.h"

#include <m+m/m+mEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the Kinect V2 special input service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::KinectV2Special;
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

KinectV2SpecialInputService::KinectV2SpecialInputService(const Utilities::DescriptorVector &
                                                                                    argumentList,
                                                         const YarpString &
                                                                                    launchPath,
                                                         const int                           argc,
                                                         char * *                            argv,
                                                         const YarpString &                  tag,
                                                         const YarpString &
                                                                                serviceEndpointName,
                                                         const YarpString &
                                                                                servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true,
              MpM_KINECTV2SPECIALINPUT_CANONICAL_NAME_, KINECTV2SPECIALINPUT_SERVICE_DESCRIPTION_,
              "", serviceEndpointName, servicePortNumber), _eventThread(NULL)
{
    ODL_ENTER(); //####
    ODL_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    ODL_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    ODL_LL1("argc = ", argc); //####
    ODL_EXIT_P(this); //####
} // KinectV2SpecialInputService::KinectV2SpecialInputService

KinectV2SpecialInputService::~KinectV2SpecialInputService(void)
{
    ODL_OBJENTER(); //####
    stopStreams();
    ODL_OBJEXIT(); //####
} // KinectV2SpecialInputService::~KinectV2SpecialInputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
DEFINE_CONFIGURE_(KinectV2SpecialInputService)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(details)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = false;

    try
    {
        result = true;
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // KinectV2SpecialInputService::configure
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

DEFINE_GETCONFIGURATION_(KinectV2SpecialInputService)
{
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    ODL_OBJEXIT_B(result); //####
    return result;
} // KinectV2SpecialInputService::getConfiguration

DEFINE_RESTARTSTREAMS_(KinectV2SpecialInputService)
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
} // KinectV2SpecialInputService::restartStreams

DEFINE_SETUPSTREAMDESCRIPTIONS_(KinectV2SpecialInputService)
{
    ODL_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");

    _outDescriptions.clear();
    description._portName = rootName + "output";
    description._portProtocol = "(d+)";
    description._protocolDescription = "A list of bodies\n"
                    "Each body being the hand state, a list of joints\n"
                    "Each joint being a validity flag, position and orientation";
    _outDescriptions.push_back(description);
    ODL_OBJEXIT_B(result); //####
    return result;
} // KinectV2SpecialInputService::setUpStreamDescriptions

DEFINE_SHUTDOWNOUTPUTSTREAMS_(KinectV2SpecialInputService)
{
    ODL_OBJENTER(); //####
    bool result = inherited::shutDownOutputStreams();

    if (_eventThread)
    {
        _eventThread->clearOutputChannel();
    }
    ODL_EXIT_B(result); //####
    return result;
} // KinectV2SpecialInputService::shutDownOutputStreams

DEFINE_STARTSERVICE_(KinectV2SpecialInputService)
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
} // KinectV2SpecialInputService::startService

DEFINE_STARTSTREAMS_(KinectV2SpecialInputService)
{
    ODL_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            _eventThread = new KinectV2SpecialEventThread(getOutletStream(0));
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
} // KinectV2SpecialInputService::startStreams

DEFINE_STOPSERVICE_(KinectV2SpecialInputService)
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
} // KinectV2SpecialInputService::stopService

DEFINE_STOPSTREAMS_(KinectV2SpecialInputService)
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
} // KinectV2SpecialInputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
