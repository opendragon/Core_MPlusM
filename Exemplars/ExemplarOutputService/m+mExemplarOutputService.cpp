//--------------------------------------------------------------------------------------------------
//
//  File:       m+mExemplarOutputService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the exemplar output service.
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
//  Created:    2014-09-15
//
//--------------------------------------------------------------------------------------------------

#include "m+mExemplarOutputService.h"

#include "m+mExemplarOutputInputHandler.h"
#include "m+mExemplarOutputRequests.h"

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
 @brief The class definition for the exemplar output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Exemplar;

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

ExemplarOutputService::ExemplarOutputService(const YarpString & launchPath,
                                             const int          argc,
                                             char * *           argv,
                                             const YarpString & tag,
                                             const YarpString & serviceEndpointName,
                                             const YarpString & servicePortNumber) :
    inherited(launchPath, argc, argv, tag, true, MpM_EXEMPLAROUTPUT_CANONICAL_NAME_,
              EXEMPLAROUTPUT_SERVICE_DESCRIPTION_, "", serviceEndpointName, servicePortNumber),
    _outFile(NULL), _inHandler(new ExemplarOutputInputHandler)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_P1("argv = ", argv); //####
    OD_LOG_EXIT_P(this); //####
} // ExemplarOutputService::ExemplarOutputService

ExemplarOutputService::~ExemplarOutputService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    delete _inHandler;
    OD_LOG_OBJEXIT(); //####
} // ExemplarOutputService::~ExemplarOutputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool ExemplarOutputService::configure(const yarp::os::Bottle & details)
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
                
                if (firstValue.isString())
                {
                    _outPath = firstValue.asString();
                    OD_LOG_S1s("_outPath <- ", _outPath); //####
                    setExtraInformation(YarpString("Output file path is '") + _outPath +
                                        YarpString("'"));
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
} // ExemplarOutputService::configure

void ExemplarOutputService::restartStreams(void)
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
} // ExemplarOutputService::restartStreams

bool ExemplarOutputService::setUpStreamDescriptions(void)
{
    OD_LOG_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");
    
    _inDescriptions.clear();
    description._portName = rootName + "input";
    description._portProtocol = "i+";
    description._protocolDescription = "One or more integer values";
    _inDescriptions.push_back(description);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // ExemplarOutputService::setUpStreamDescriptions

bool ExemplarOutputService::start(void)
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
} // ExemplarOutputService::start

void ExemplarOutputService::startStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            _outFile = fopen(_outPath.c_str(), "w");
            if (_outFile)
            {
                if (_inHandler)
                {
                    _inHandler->setFile(_outFile);
                    getInletStream(0)->setReader(*_inHandler);
                    setActive();
                }
                else
                {
                    fclose(_outFile);
                    _outFile = NULL;
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
} // ExemplarOutputService::startStreams

bool ExemplarOutputService::stop(void)
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
} // ExemplarOutputService::stop

void ExemplarOutputService::stopStreams(void)
{
    OD_LOG_OBJENTER(); //####
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
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // ExemplarOutputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
