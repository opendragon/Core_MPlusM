//--------------------------------------------------------------------------------------
//
//  File:       M+MRecordIntegersStreamService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a simple M+M output service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-06-24
//
//--------------------------------------------------------------------------------------

#include "M+MRecordIntegersStreamService.h"
#include "M+MGeneralChannel.h"
#include "M+MRecordIntegersInputHandler.h"
#include "M+MRecordIntegersStreamRequests.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for a simple M+M output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;

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
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

RecordIntegersStreamService::RecordIntegersStreamService(const yarp::os::ConstString & launchPath,
                                                         const yarp::os::ConstString & serviceEndpointName,
                                                         const yarp::os::ConstString & servicePortNumber) :
        inherited(launchPath, true, MpM_RECORDINTEGERS_CANONICAL_NAME, "An example record integers output service", "",
                  serviceEndpointName, servicePortNumber), _outFile(NULL), _inHandler(new RecordIntegersInputHandler)
{
    OD_LOG_ENTER();//####
    OD_LOG_S3("launchPath = ", launchPath.c_str(), "serviceEndpointName = ", serviceEndpointName.c_str(),//####
              "servicePortNumber = ", servicePortNumber.c_str());//####
    OD_LOG_EXIT_P(this);//####
} // RecordIntegersStreamService::RecordIntegersStreamService

RecordIntegersStreamService::~RecordIntegersStreamService(void)
{
    OD_LOG_OBJENTER();//####
    stopStreams();
    delete _inHandler;
    OD_LOG_OBJEXIT();//####
} // RecordIntegersStreamService::~RecordIntegersStreamService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool RecordIntegersStreamService::configure(const Common::Package & details)
{
    OD_LOG_OBJENTER();//####
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
                    OD_LOG_S1("_outPath <- ", _outPath.c_str());//####
                    result = true;
                }
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // RecordIntegersStreamService::configure

void RecordIntegersStreamService::restartStreams(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        // No special processing needed.
        stopStreams();
        startStreams();
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RecordIntegersStreamService::restartStreams

bool RecordIntegersStreamService::setUpStreamDescriptions(void)
{
    OD_LOG_OBJENTER();//####
    bool                       result = true;
    Common::ChannelDescription description;
    
    _inDescriptions.clear();
    description._portName = "example/recordintegersstream/input_";
    description._portProtocol = "i+";
    _inDescriptions.push_back(description);
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // RecordIntegersStreamService::setUpStreamDescriptions

bool RecordIntegersStreamService::start(void)
{
    OD_LOG_OBJENTER();//####
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
                OD_LOG("! (isStarted())");//####
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(isStarted());//####
    return isStarted();
} // RecordIntegersStreamService::start

void RecordIntegersStreamService::startStreams(void)
{
    OD_LOG_OBJENTER();//####
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
                    _inStreams.at(0)->setReader(*_inHandler);
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
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RecordIntegersStreamService::startStreams

bool RecordIntegersStreamService::stop(void)
{
    OD_LOG_OBJENTER();//####
    bool result;
    
    try
    {
        result = inherited::stop();
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // RecordIntegersStreamService::stop

void RecordIntegersStreamService::stopStreams(void)
{
    OD_LOG_OBJENTER();//####
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
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RecordIntegersStreamService::stopStreams
