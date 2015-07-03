//--------------------------------------------------------------------------------------------------
//
//  File:       m+mProComp2InputThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a thread that generates output from ProComp2 data.
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
//  Created:    2015-04-16
//
//--------------------------------------------------------------------------------------------------

#include "m+mProComp2InputThread.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#define _WIN32_DCOM // for using CoInitializeEx

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4996)
#endif // ! MAC_OR_LINUX_
#define USING_WRAPPER_CLASS
#include "ttllive.h"
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

/*! @brief The sample timer interval. */
#define PROCOMP2_TIMER_INTERVAL_ 100

//#define USE_VARIANT_READ_METHOD_ /* if defined, use a Variant-based read. */

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a thread that generates output from %ProComp2 data. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::ProComp2;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

static ITTLLive2Ptr lTTLLive;

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

static void show_error(_com_error & ee)
{
	cerr << "(0x" << std::hex << ee.Error() << std::dec << ")" << endl;
	cerr << static_cast<char *>(ee.Description()) << endl;
} // show_error

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

ProComp2InputThread::ProComp2InputThread(GeneralChannel * outChannel) :
    inherited(), _outChannel(outChannel)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("outChannel = ", outChannel); //####
    OD_LOG_EXIT_P(this); //####
} // ProComp2InputThread::ProComp2InputThread

ProComp2InputThread::~ProComp2InputThread(void)
{
    OD_LOG_OBJENTER(); //####
	OD_LOG_OBJEXIT(); //####
} // ProComp2InputThread::~ProComp2InputThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void ProComp2InputThread::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = nullptr;
    OD_LOG_OBJEXIT(); //####
} // ProComp2InputThread::clearOutputChannel

void ProComp2InputThread::readChannelData(const DWORD time)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_LL1("time = ", time); //####
    bool                 sawData = false;
    char                 tag[2];
    LONG                 samplesAvailable;
    yarp::os::Bottle     message;
    yarp::os::Property & props = message.addDict();
#if defined(USE_VARIANT_READ_METHOD_)
    _variant_t           variant;
    SAFEARRAY *          pSA = nullptr;
#else // ! defined(USE_VARIANT_READ_METHOD_)
    FLOAT                buffer[4096];
#endif // ! defined(USE_VARIANT_READ_METHOD_)

    memset(tag, '\0', sizeof(tag));
#if defined(USE_VARIANT_READ_METHOD_)
    for (LONG channelHND = lTTLLive->GetFirstChannelHND(); -1 < channelHND;
         channelHND = lTTLLive->GetNextChannelHND())
    {
        samplesAvailable = lTTLLive->SamplesAvailable[channelHND];
        if (samplesAvailable)
        {
            variant = lTTLLive->ReadChannelDataVT(channelHND, samplesAvailable);
            
            if (VT_ARRAY == (variant.vt & VT_ARRAY))
            {
                if (VT_R4 == (variant.vt & VT_R4))
                {
                    pSA = ((variant.vt & VT_BYREF) ? *(variant.pparray) : variant.parray);
                    if (S_OK == ::SafeArrayLock(pSA))
                    {
                        samplesAvailable = pSA->rgsabound[0].cElements;
                        ::SafeArrayUnlock(pSA);
                    }
                    else
                    {
                        samplesAvailable = 0;
                    }
                }
            }
        }
        // If any samples available, we simply print out value of the first one
        if (samplesAvailable)
        {
            if (S_OK == ::SafeArrayLock(pSA))
            {
                FLOAT * data = reinterpret_cast<FLOAT *>(pSA->pvData);
                
                ::SafeArrayUnlock(pSA);
                tag[0] = static_cast<char>('A' + channelHND);
                sawData = true;
                props.put(tag, data[0]);
            }
        }
    }
#else // ! defined(USE_VARIANT_READ_METHOD_)
    for (LONG channelHND = lTTLLive->GetFirstChannelHND(); -1 < channelHND;
         channelHND = lTTLLive->GetNextChannelHND())
    {
        samplesAvailable = lTTLLive->SamplesAvailable[channelHND];
        if (samplesAvailable)
        {
            lTTLLive->ReadChannelData(channelHND, buffer, &samplesAvailable);
        }
        // If any samples available, we simply print out value of the first one
        if (samplesAvailable)
        {
            tag[0] = static_cast<char>('A' + channelHND);
            sawData = true;
            props.put(tag, buffer[0]);
        }
    }
#endif // ! defined(USE_VARIANT_READ_METHOD_)
    if (sawData && _outChannel)
    {
        props.put("time", static_cast<double>(time));
        if (! _outChannel->write(message))
        {
            OD_LOG("(! _outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
            Stall();
#endif // defined(MpM_StallOnSendProblem)
        }
    }
    OD_LOG_OBJEXIT(); //####
} // ProComp2InputThread::readChannelData

void ProComp2InputThread::run(void)
{
    OD_LOG_OBJENTER(); //####
    MSG      aMessage;
    UINT_PTR aTimer = nullptr;
    
    memset(&aMessage, 0, sizeof(aMessage));
    if (0 < lTTLLive->EncoderCount)
    {
        lTTLLive->StartChannels();
        // Set up a Windows time.
        aTimer = ::SetTimer(nullptr, 0, PROCOMP2_TIMER_INTERVAL_, nullptr);
    }
    for ( ; ! isStopping(); )
    {
        while (PeekMessage(&aMessage, nullptr, 0, 0, PM_REMOVE))
        {
            if (WM_TIMER == aMessage.message)
            {
                if (aMessage.wParam == aTimer)
                {
                    readChannelData(aMessage.time);
                }
            }
            DispatchMessage(&aMessage);
        }
        yarp::os::Time::yield();
    }
    lTTLLive->StopChannels();
    ::KillTimer(nullptr, aTimer);
    if (lTTLLive)
    {
        lTTLLive.Release();
    }
	OD_LOG_OBJEXIT(); //####
} // ProComp2InputThread::run

bool ProComp2InputThread::setupEncoders(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = true;
    
    OD_LOG("Autodetecting encoders"); //####
    try
    {
        lTTLLive->OpenConnections(TTLAPI_OCCMD_AUTODETECT, 1000, nullptr, nullptr);
        OD_LOG("Setting up channels"); //####
        lTTLLive->AutoSetupChannels();
        for (LONG channelHND = lTTLLive->GetFirstChannelHND(); -1 < channelHND;
             channelHND = lTTLLive->GetNextChannelHND())
        {
            // Setting up channels with arbitrary configuration, turning-off channel notification,
            // pluggin-in sensor ID into sensor type and set channels to output raw COUNTS.
            lTTLLive->Notification[channelHND] = 0;
            lTTLLive->ForceSensor[channelHND] = false;
            lTTLLive->SensorType[channelHND] = lTTLLive->SensorID[channelHND];
            lTTLLive->UnitType[channelHND] = TTLAPI_UT_COUNT;
        }
        result = (0 < lTTLLive->EncoderCount);
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        result = false;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // ProComp2InputThread::setupEncoders

bool ProComp2InputThread::threadInit(void)
{
    OD_LOG_OBJENTER(); //####
    bool    result = true;
	HRESULT hr = CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED);
	
	if (SUCCEEDED(hr))
	{
        OD_LOG("Creating instance"); //####
		hr = lTTLLive.CreateInstance(CLSID_TTLLive);
		if (SUCCEEDED(hr))
		{
			try
			{
                T_Version sV;
             
                OD_LOG("Getting version"); //####
                sV.liVersion = lTTLLive->Version;
                cout << "TTL Version = " << static_cast<int>(sV.byMajor) << "." <<
                        static_cast<int>(sV.byMinor) << "." << static_cast<int>(sV.woBuild) << endl;
                result = setupEncoders();
            }
			catch (_com_error & ee)
			{
				OD_LOG("_com_error caught"); //####
				show_error(ee);
				result = false;
			}
			catch (...)
			{
				OD_LOG("Exception caught"); //####
				throw;
			}
		}
		else
		{
			OD_LOG("! (SUCCEEDED(hr))"); //####
			CheckHRESULT(hr);
			result = false;
		}
	}
	else
	{
		OD_LOG("! (SUCCEEDED(hr)"); //####
		CheckHRESULT(hr);
#if MAC_OR_LINUX_
		GetLogger().fail("CoInitializeEx() failed.");
#else // ! MAC_OR_LINUX_
		cerr << "CoInitializeEx() failed." << endl;
#endif // ! MAC_OR_LINUX_
	}
	if (! result)
	{
		if (lTTLLive)
		{
			lTTLLive.Release();
		}
	}
	OD_LOG_OBJEXIT_B(result); //####
    return result;
} // ProComp2InputThread::threadInit

void ProComp2InputThread::threadRelease(void)
{
    OD_LOG_OBJENTER(); //####
	CoUninitialize();
	OD_LOG_OBJEXIT(); //####
} // ProComp2InputThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
