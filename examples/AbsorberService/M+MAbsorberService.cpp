//--------------------------------------------------------------------------------------------------
//
//  File:       M+MAbsorberService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the Absorber output service.
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
//  Created:    2014-09-30
//
//--------------------------------------------------------------------------------------------------

#include "M+MAbsorberService.h"
#include "M+MAbsorberInputHandler.h"
#include "M+MAbsorberRequests.h"

#include <mpm/M+MEndpoint.h>
#include <mpm/M+MGeneralChannel.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the Absorber output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Convert a positive numeric value into a comma-separated string.
 @param aNumber The number to convert.
 @returns A string representation of the number with commas between each triple digit group. */
static yarp::os::ConstString convertToCommaSplitNumber(const size_t aNumber)
{
    // Note that the function 'convertToCommaSplitNumber' was used because the locale
    // mechanism in C++ for OS X is very, very broken - so we can't use 'std::imbue'!
    yarp::os::ConstString result;
    size_t                work = aNumber;
    
    do
    {
        size_t            bottom = (work % 1000);
        std::stringstream buff;
        
        if (1000 <= work)
        {
            buff.width(3);
            buff.fill('0');
        }
        buff << bottom;
        result = yarp::os::ConstString(buff.str()) + result;
        work /= 1000;
        if (0 < work)
        {
            result = yarp::os::ConstString(",") + result;
        }
    }
    while (0 < work);
    return result;
} // convertToCommaSplitNumber

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

AbsorberService::AbsorberService(const yarp::os::ConstString & launchPath,
                                 const yarp::os::ConstString & tag,
                                 const yarp::os::ConstString & serviceEndpointName,
                                 const yarp::os::ConstString & servicePortNumber) :
    inherited(launchPath, tag, true, MpM_ABSORBER_CANONICAL_NAME, "The Absorber output service",
              "", serviceEndpointName, servicePortNumber),
    _inHandler(new AbsorberInputHandler(*this)), _count(0), _totalBytes(0)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_EXIT_P(this); //####
} // AbsorberService::AbsorberService

AbsorberService::~AbsorberService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    delete _inHandler;
    OD_LOG_OBJEXIT(); //####
} // AbsorberService::~AbsorberService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool AbsorberService::configure(const yarp::os::Bottle & details)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = false;
    
    try
    {
        // Nothing needs to be done.
        result = true;
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // AbsorberService::configure

void AbsorberService::restartStreams(void)
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
} // AbsorberService::restartStreams

bool AbsorberService::setUpStreamDescriptions(void)
{
    OD_LOG_OBJENTER(); //####
    bool                  result = true;
    ChannelDescription    description;
    yarp::os::ConstString rootName(getEndpoint().getName() + "/");
    
    _inDescriptions.clear();
    description._portName = rootName + "input";
    description._portProtocol = ""; // Empty, so everything accepted
    description._protocolDescription = "";
    _inDescriptions.push_back(description);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // AbsorberService::setUpStreamDescriptions

bool AbsorberService::start(void)
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
} // AbsorberService::start

void AbsorberService::startStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_inHandler)
            {
                _inStreams.at(0)->setReader(*_inHandler);
                setActive();
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // AbsorberService::startStreams

bool AbsorberService::stop(void)
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
} // AbsorberService::stop

void AbsorberService::stopStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            clearActive();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // AbsorberService::stopStreams

void AbsorberService::updateCount(const size_t numBytes)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_L1("numBytes = ", numBytes); //####
    try
    {
        if (isActive())
        {
            ++_count;
            _totalBytes += numBytes;
            // Note that the function 'convertToCommaSplitNumber' was used because the locale
            // mechanism in C++ for OS X is very, very broken.
            cout << "messages = " << convertToCommaSplitNumber(_count).c_str() << "; bytes = " <<
                    convertToCommaSplitNumber(numBytes).c_str() << "; total = " <<
                    convertToCommaSplitNumber(_totalBytes).c_str() << endl;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // AbsorberService::updateCount

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
