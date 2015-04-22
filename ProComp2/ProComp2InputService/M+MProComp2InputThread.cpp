//--------------------------------------------------------------------------------------------------
//
//  File:       M+MProComp2InputThread.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for an output-generating thread for M+M.
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

#include "M+MProComp2InputThread.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#define _WIN32_DCOM			// for using CoInitializeEx

#define USING_WRAPPER_CLASS
#include "ttllive.h"

#define REAL_POINTER(vv) reinterpret_cast<ITTLLive2Ptr *>(vv)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for an output-generating thread for M+M. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
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

ProComp2InputThread::ProComp2InputThread(GeneralChannel * outChannel/*,
                                         const double     timeToWait,
                                         const int        numValues*/) :
    inherited(), _outChannel(outChannel), _TTLLive(nullptr)//, _timeToWait(timeToWait), _numValues(numValues)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("outChannel = ", outChannel); //####
    //OD_LOG_D1("timeToWait = ", timeToWait); //####
    //OD_LOG_LL1("numValues = ", numValues); //####
	_TTLLive = new ITTLLive2Ptr;
    OD_LOG_EXIT_P(this); //####
} // ProComp2InputThread::ProComp2InputThread

ProComp2InputThread::~ProComp2InputThread(void)
{
    OD_LOG_OBJENTER(); //####
	delete _TTLLive;
	OD_LOG_OBJEXIT(); //####
} // ProComp2InputThread::~ProComp2InputThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void ProComp2InputThread::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = NULL;
    OD_LOG_OBJEXIT(); //####
} // ProComp2InputThread::clearOutputChannel

void ProComp2InputThread::run(void)
{
    OD_LOG_OBJENTER(); //####
    for ( ; ! isStopping(); )
    {
#if 0
        if (_nextTime <= yarp::os::Time::now())
        {
            OD_LOG("(_nextTime <= yarp::os::Time::now())"); //####
            yarp::os::Bottle message;
            
            for (int ii = 0; ii < _numValues; ++ii)
            {
                message.addDouble(10000 * yarp::os::Random::uniform());
            }
            if (_outChannel)
            {
                if (! _outChannel->write(message))
                {
                    OD_LOG("(! _outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                    Stall();
#endif // defined(MpM_StallOnSendProblem)
                }
            }
            _nextTime = yarp::os::Time::now() + _timeToWait;
        }
#endif//0
        yarp::os::Time::yield();
    }
	OD_LOG_OBJEXIT(); //####
} // ProComp2InputThread::run

bool ProComp2InputThread::threadInit(void)
{
    OD_LOG_OBJENTER(); //####
    bool    result = true;
	HRESULT hr = CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
	
	if (SUCCEEDED(hr))
	{
		//    _nextTime = yarp::os::Time::now() + _timeToWait;
		hr = REAL_POINTER(_TTLLive)->CreateInstance(CLSID_TTLLive);
		if (SUCCEEDED(hr))
		{
			try
			{
				T_Version sV;

				sV.liVersion = (*REAL_POINTER(_TTLLive))->Version;
				cout << "TTL Version = " << static_cast<int>(sV.byMajor) << "." << static_cast<int>(sV.byMinor) << "." << static_cast<int>(sV.woBuild) << endl;
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
