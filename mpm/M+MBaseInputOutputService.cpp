//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseInputOutputService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the minimal functionality required for an M+M input/output
//              service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-06-23
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MBaseInputOutputService.h>
#include <mpm/M+MGeneralChannel.h>

#include "M+MConfigureRequestHandler.h"
#include "M+MRestartStreamsRequestHandler.h"
#include "M+MStartStreamsRequestHandler.h"
#include "M+MStopStreamsRequestHandler.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the minimal functionality required for an M+M input/output
 service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

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
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

BaseInputOutputService::BaseInputOutputService(const ServiceKind             theKind,
                                               const yarp::os::ConstString & launchPath,
                                               const yarp::os::ConstString & tag,
                                               const bool                    useMultipleHandlers,
                                               const yarp::os::ConstString & canonicalName,
                                               const yarp::os::ConstString & description,
                                               const yarp::os::ConstString & requestsDescription,
                                               const yarp::os::ConstString & serviceEndpointName,
                                               const yarp::os::ConstString & servicePortNumber) :
    inherited(theKind, launchPath, tag, useMultipleHandlers, canonicalName, description,
              requestsDescription, serviceEndpointName, servicePortNumber), _active(false)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "canonicalName = ", canonicalName, //####
               "description = ", description); //####
    OD_LOG_S3s("requestsDescription = ", requestsDescription, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_B1("useMultipleHandlers = ", useMultipleHandlers); //####
    attachRequestHandlers();
    OD_LOG_EXIT_P(this); //####
} // BaseInputOutputService::BaseInputOutputService

BaseInputOutputService::~BaseInputOutputService(void)
{
    OD_LOG_OBJENTER(); //####
    shutDownInputStreams();
    shutDownOutputStreams();
    detachRequestHandlers();
    OD_LOG_OBJEXIT(); //####
} // BaseInputOutputService::~BaseInputOutputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool BaseInputOutputService::addInStreamsFromDescriptions(const ChannelVector & descriptions)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("descriptions = ", &descriptions); //####
    bool result = true;
    
    try
    {
        if (0 < descriptions.size())
        {
#if defined(MpM_ReportOnConnections)
            ChannelStatusReporter * reporter = Utilities::GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)

            for (ChannelVector::const_iterator walker(descriptions.begin());
                 result && (descriptions.end() != walker); ++walker)
            {
                GeneralChannel * newChannel = new GeneralChannel(false);
                
                if (newChannel)
                {
                    ChannelDescription aDescription(*walker);
                    
#if defined(MpM_ReportOnConnections)
                    newChannel->setReporter(reporter);
                    newChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
                    if (newChannel->openWithRetries(aDescription._portName, STANDARD_WAIT_TIME))
                    {
                        newChannel->setProtocol(aDescription._portProtocol,
                                                aDescription._protocolDescription);
                        if (metricsAreEnabled())
                        {
                            newChannel->enableMetrics();
                        }
                        else
                        {
                            newChannel->disableMetrics();
                        }
                        _inStreams.push_back(newChannel);
                    }
                    else
                    {
                        OD_LOG("! (newChannel->openWithRetries(aDescription._portName, " //####
                               "STANDARD_WAIT_TIME))"); //####
#if MAC_OR_LINUX_
                        GetLogger().fail("Problem opening input channel.");
#else // ! MAC_OR_LINUX_
                        std::cerr << "Problem opening input channel." << std::endl;
#endif // ! MAC_OR_LINUX_
                        result = false;
                    }
                }
                else
                {
                    OD_LOG("! (newChannel)"); //####
                    result = false;
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
} // BaseInputOutputService::addInStreamsFromDescriptions

bool BaseInputOutputService::addOutStreamsFromDescriptions(const ChannelVector & descriptions)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("descriptions = ", &descriptions); //####
    bool result = true;
    
    try
    {
        if (0 < descriptions.size())
        {
#if defined(MpM_ReportOnConnections)
            ChannelStatusReporter * reporter = Utilities::GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)
            
            for (ChannelVector::const_iterator walker(descriptions.begin());
                 result && (descriptions.end() != walker); ++walker)
            {
                GeneralChannel * newChannel = new GeneralChannel(true);
                
                if (newChannel)
                {
                    ChannelDescription aDescription(*walker);
                    
#if defined(MpM_ReportOnConnections)
                    newChannel->setReporter(reporter);
                    newChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
                    if (newChannel->openWithRetries(aDescription._portName, STANDARD_WAIT_TIME))
                    {
                        newChannel->setProtocol(aDescription._portProtocol,
                                                aDescription._protocolDescription);
                        if (metricsAreEnabled())
                        {
                            newChannel->enableMetrics();
                        }
                        else
                        {
                            newChannel->disableMetrics();
                        }
                        _outStreams.push_back(newChannel);
                    }
                    else
                    {
                        OD_LOG("! (newChannel->openWithRetries(newName, " //####
                               "STANDARD_WAIT_TIME))"); //####
#if MAC_OR_LINUX_
                        GetLogger().fail("Problem opening output channel.");
#else // ! MAC_OR_LINUX_
                        std::cerr << "Problem opening output channel." << std::endl;
#endif // ! MAC_OR_LINUX_
                        result = false;
                    }
                }
                else
                {
                    OD_LOG("! (newChannel)"); //####
                    result = false;
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
} // BaseInputOutputService::addOutStreamsFromDescriptions

void BaseInputOutputService::attachRequestHandlers(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        _configureHandler = new ConfigureRequestHandler(*this);
        _restartStreamsHandler = new RestartStreamsRequestHandler(*this);
        _startStreamsHandler = new StartStreamsRequestHandler(*this);
        _stopStreamsHandler = new StopStreamsRequestHandler(*this);
        if (_configureHandler && _restartStreamsHandler && _startStreamsHandler &&
            _stopStreamsHandler)
        {
            registerRequestHandler(_configureHandler);
            registerRequestHandler(_restartStreamsHandler);
            registerRequestHandler(_startStreamsHandler);
            registerRequestHandler(_stopStreamsHandler);
        }
        else
        {
            OD_LOG("! (_configureHandler && _restartStreamsHandler && " //####
                   "_startStreamsHandler && _stopStreamsHandler)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // BaseInputOutputService::attachRequestHandlers

void BaseInputOutputService::detachRequestHandlers(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (_configureHandler)
        {
            unregisterRequestHandler(_configureHandler);
            delete _configureHandler;
            _configureHandler = NULL;
        }
        if (_restartStreamsHandler)
        {
            unregisterRequestHandler(_restartStreamsHandler);
            delete _restartStreamsHandler;
            _restartStreamsHandler = NULL;
        }
        if (_startStreamsHandler)
        {
            unregisterRequestHandler(_startStreamsHandler);
            delete _startStreamsHandler;
            _startStreamsHandler = NULL;
        }
        if (_stopStreamsHandler)
        {
            unregisterRequestHandler(_stopStreamsHandler);
            delete _stopStreamsHandler;
            _stopStreamsHandler = NULL;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // BaseInputOutputService::detachRequestHandlers

void BaseInputOutputService::disableMetrics(void)
{
    OD_LOG_OBJENTER(); //####
    inherited::disableMetrics();
    if (0 < _inStreams.size())
    {
        for (StreamVector::const_iterator walker(_inStreams.begin()); _inStreams.end() != walker;
             ++walker)
        {
            GeneralChannel * aChannel = *walker;
            
            if (aChannel)
            {
                aChannel->disableMetrics();
            }
        }
    }
    if (0 < _outStreams.size())
    {
        for (StreamVector::const_iterator walker(_outStreams.begin()); _outStreams.end() != walker;
             ++walker)
        {
            GeneralChannel * aChannel = *walker;
            
            if (aChannel)
            {
                aChannel->disableMetrics();
            }
        }
    }
    OD_LOG_OBJEXIT(); //####
} // BaseInputOutputService::disableMetrics

void BaseInputOutputService::enableMetrics(void)
{
    OD_LOG_OBJENTER(); //####
    inherited::enableMetrics();
    if (0 < _inStreams.size())
    {
        for (StreamVector::const_iterator walker(_inStreams.begin()); _inStreams.end() != walker;
             ++walker)
        {
            GeneralChannel * aChannel = *walker;
            
            if (aChannel)
            {
                aChannel->enableMetrics();
            }
        }
    }
    if (0 < _outStreams.size())
    {
        for (StreamVector::const_iterator walker(_outStreams.begin()); _outStreams.end() != walker;
             ++walker)
        {
            GeneralChannel * aChannel = *walker;
            
            if (aChannel)
            {
                aChannel->enableMetrics();
            }
        }
    }
    OD_LOG_OBJEXIT(); //####
} // BaseInputOutputService::enableMetrics

void BaseInputOutputService::fillInSecondaryInputChannelsList(ChannelVector & channels)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("channels = ", &channels); //####
    inherited::fillInSecondaryInputChannelsList(channels);
    if (0 < _inStreams.size())
    {
        for (StreamVector::const_iterator walker(_inStreams.begin()); _inStreams.end() != walker;
             ++walker)
        {
            GeneralChannel * aChannel = *walker;
            
            if (aChannel)
            {
                OD_LOG_S1s("aChannel = ", aChannel->name()); //####
                ChannelDescription descriptor;
                
                descriptor._portName = aChannel->name();
                descriptor._portProtocol = aChannel->protocol();
                descriptor._portMode = kChannelModeTCP;
                descriptor._protocolDescription = aChannel->protocolDescription();
                channels.push_back(descriptor);
            }
        }
    }
    OD_LOG_OBJEXIT(); //####
} // BaseInputOutputService::fillInSecondaryInputChannelsList

void BaseInputOutputService::fillInSecondaryOutputChannelsList(ChannelVector & channels)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("channels = ", &channels); //####
    inherited::fillInSecondaryOutputChannelsList(channels);
    if (0 < _outStreams.size())
    {
        for (StreamVector::const_iterator walker(_outStreams.begin()); _outStreams.end() != walker;
             ++walker)
        {
            GeneralChannel * aChannel = *walker;
            
            if (aChannel)
            {
                OD_LOG_S1s("aChannel = ", aChannel->name()); //####
                ChannelDescription descriptor;
                
                descriptor._portName = aChannel->name();
                descriptor._portProtocol = aChannel->protocol();
                descriptor._portMode = kChannelModeTCP;
                descriptor._protocolDescription = aChannel->protocolDescription();
                channels.push_back(descriptor);
            }
        }
    }
    OD_LOG_OBJEXIT(); //####
} // BaseInputOutputService::fillInSecondaryOutputChannelsList

void BaseInputOutputService::gatherMetrics(yarp::os::Bottle & metrics)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("metrics = ", &metrics); //####
    SendReceiveCounters counters;
    
    inherited::gatherMetrics(metrics);
    for (StreamVector::const_iterator walker(_inStreams.begin()); _inStreams.end() != walker;
         ++walker)
    {
        GeneralChannel * aChannel = *walker;
        
        if (aChannel)
        {
            aChannel->getSendReceiveCounters(counters);
            counters.addToList(metrics, aChannel->name());
        }
    }
    for (StreamVector::const_iterator walker(_outStreams.begin()); _outStreams.end() != walker;
         ++walker)
    {
        GeneralChannel * aChannel = *walker;
        
        if (aChannel)
        {
            aChannel->getSendReceiveCounters(counters);
            counters.addToList(metrics, aChannel->name());
        }
    }
    OD_LOG_OBJEXIT(); //####
} // BaseInputOutputService::gatherMetrics

bool BaseInputOutputService::setUpInputStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = shutDownInputStreams(); // clear out existing streams first
    
    OD_LOG_EXIT_B(result); //####
    return result;
} // BaseInputOutputService::setUpInputStreams

bool BaseInputOutputService::setUpOutputStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = shutDownOutputStreams(); // clear out existing streams first
    
    OD_LOG_EXIT_B(result); //####
    return result;
} // BaseInputOutputService::setUpOutputStreams

bool BaseInputOutputService::shutDownInputStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = true; // by default, always true
    
    if (0 < _inStreams.size())
    {
        for (StreamVector::const_iterator walker(_inStreams.begin()); _inStreams.end() != walker;
             ++walker)
        {
            GeneralChannel * aChannel = *walker;
            
            if (aChannel)
            {
                OD_LOG_P1("aChannel = ", aChannel); //####
                delete aChannel;
            }
        }
        _inStreams.clear();
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // BaseInputOutputService::shutDownInputStreams

bool BaseInputOutputService::shutDownOutputStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = true; // by default, always true
    
    if (0 < _outStreams.size())
    {
        for (StreamVector::const_iterator walker(_outStreams.begin()); _outStreams.end() != walker;
             ++walker)
        {
            GeneralChannel * aChannel = *walker;
            
            if (aChannel)
            {
                OD_LOG_P1("aChannel = ", aChannel); //####
                delete aChannel;
            }
        }
        _outStreams.clear();
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // BaseInputOutputService::shutDownOutputStreams

bool BaseInputOutputService::start(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = false;
    
    try
    {
        if (! isStarted())
        {
            inherited::start();
            if (isStarted() && setUpStreamDescriptions() && setUpInputStreams() &&
                setUpOutputStreams())
            {
            
            }
            else
            {
                OD_LOG("! (isStarted() && setUpStreamDescriptions() && " //####
                       "setUpInputStreams() && setUpOutputStreams())"); //####
            }
        }
        result = isStarted();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // BaseInputOutputService::start

bool BaseInputOutputService::stop(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = true;
    
    try
    {
        if (! shutDownInputStreams())
        {
            result = false;
        }
        if (! shutDownOutputStreams())
        {
            result = false;
        }
        if (! inherited::stop())
        {
            result = false;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // BaseInputOutputService::stop

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
