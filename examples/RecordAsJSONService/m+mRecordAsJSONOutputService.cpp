//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRecordAsJSONOutputService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Record As JSON output service.
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
//  Created:    2014-11-26
//
//--------------------------------------------------------------------------------------------------

#include "m+mRecordAsJSONOutputService.h"

#include "m+mRecordAsJSONOutputInputHandler.h"
#include "m+mRecordAsJSONOutputRequests.h"

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
 @brief The class definition for the Record As JSON output service. */
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

RecordAsJSONOutputService::RecordAsJSONOutputService(const Utilities::DescriptorVector &
                                                                                     argumentList,
                                                     const YarpString &                  launchPath,
                                                     const int                           argc,
                                                     char * *                            argv,
                                                     const YarpString &                  tag,
                                                     const YarpString &
                                                                             serviceEndpointName,
                                                     const YarpString &
                                                                             servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true,
              MpM_RECORDASJSONOUTPUT_CANONICAL_NAME_, RECORDASJSONOUTPUT_SERVICE_DESCRIPTION_, "",
              serviceEndpointName, servicePortNumber),
    _outFile(NULL), _inHandler(new RecordAsJSONOutputInputHandler)
{
    ODL_ENTER(); //####
    ODL_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    ODL_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    ODL_LL1("argc = ", argc); //####
    ODL_EXIT_P(this); //####
} // RecordAsJSONOutputService::RecordAsJSONOutputService

RecordAsJSONOutputService::~RecordAsJSONOutputService(void)
{
    ODL_OBJENTER(); //####
    stopStreams();
    delete _inHandler;
    ODL_OBJEXIT(); //####
} // RecordAsJSONOutputService::~RecordAsJSONOutputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

DEFINE_CONFIGURE_(RecordAsJSONOutputService)
{
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = false;

    try
    {
        if (1 <= details.size())
        {
            yarp::os::Value firstValue(details.get(0));

            if (firstValue.isString())
            {
                _outPath = firstValue.asString();
                ODL_S1s("_outPath <- ", _outPath); //####
                setExtraInformation(YarpString("Output file path is '") + _outPath +
                                    YarpString("'"));
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
} // RecordAsJSONOutputService::configure

DEFINE_DISABLEMETRICS_(RecordAsJSONOutputService)
{
    ODL_OBJENTER(); //####
    inherited::disableMetrics();
    if (_inHandler)
    {
        _inHandler->disableMetrics();
    }
    ODL_OBJEXIT(); //####
} // RecordAsJSONOutputService::disableMetrics

DEFINE_ENABLEMETRICS_(RecordAsJSONOutputService)
{
    ODL_OBJENTER(); //####
    inherited::enableMetrics();
    if (_inHandler)
    {
        _inHandler->enableMetrics();
    }
    ODL_OBJEXIT(); //####
} // RecordAsJSONOutputService::enableMetrics

DEFINE_GETCONFIGURATION_(RecordAsJSONOutputService)
{
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    details.addString(_outPath);
    ODL_OBJEXIT_B(result); //####
    return result;
} // RecordAsJSONOutputService::getConfiguration

DEFINE_RESTARTSTREAMS_(RecordAsJSONOutputService)
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
} // RecordAsJSONOutputService::restartStreams

DEFINE_SETUPSTREAMDESCRIPTIONS_(RecordAsJSONOutputService)
{
    ODL_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");

    _inDescriptions.clear();
    description._portName = rootName + "input";
    description._portProtocol = "*";
    description._protocolDescription = "Arbitrary YARP messages";
    _inDescriptions.push_back(description);
    ODL_OBJEXIT_B(result); //####
    return result;
} // RecordAsJSONOutputService::setUpStreamDescriptions

DEFINE_STARTSERVICE_(RecordAsJSONOutputService)
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
} // RecordAsJSONOutputService::startService

DEFINE_STARTSTREAMS_(RecordAsJSONOutputService)
{
    ODL_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            int why;

#if MAC_OR_LINUX_
            _outFile = fopen(_outPath.c_str(), "w");
            why = errno;
#else // ! MAC_OR_LINUX_
            why = fopen_s(&_outFile, _outPath.c_str(), "w");
            if (why)
            {
                _outFile = NULL;
            }
#endif // ! MAC_OR_LINUX_
            if (_outFile)
            {
                if (_inHandler)
                {
                    _inHandler->setFile(_outFile);
                    _inHandler->setChannel(getInletStream(0));
                    getInletStream(0)->setReader(*_inHandler);
                    setActive();
                }
                else
                {
                    fclose(_outFile);
                    _outFile = NULL;
                }
            }
            else
            {
                cerr << "Could not open file '" << _outPath.c_str() <<
                        "' for writing, error code = " << why << "." << endl;
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RecordAsJSONOutputService::startStreams

DEFINE_STOPSERVICE_(RecordAsJSONOutputService)
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
} // RecordAsJSONOutputService::stopService

DEFINE_STOPSTREAMS_(RecordAsJSONOutputService)
{
    ODL_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            if (_inHandler)
            {
                _inHandler->setFile(NULL);
            }
            fclose(_outFile);
            _outFile = NULL;
            clearActive();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RecordAsJSONOutputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
