//--------------------------------------------------------------------------------------------------
//
//  File:       M+MViconDataStreamEventThread.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a thread that generates output from Vicon data.
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
//  Created:    2014-11-07
//
//--------------------------------------------------------------------------------------------------

#include "M+MViconDataStreamEventThread.h"

#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a thread that generates output from Vicon data. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::ViconDataStream;
using namespace ViconDataStreamSDK;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

//#define VICON_Y_UP

#define VICON_Z_UP

//#define USE_SEGMENT_LOCAL_DATA

//#define VICON_STREAM_MODE CPP::StreamMode::ClientPullPreFetch
#define VICON_STREAM_MODE CPP::StreamMode::ServerPush

/*! @brief The number of times we attempt to connect to the Vicon device server. */
static const int kNumConnectTries = 17;

/*! @brief The amount of time to sleep between network operations that need to be retried. */
static const int kLittleSleep = 200;

//#define REPORT_EVENT_COUNT_ /* */

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

ViconDataStreamEventThread::ViconDataStreamEventThread(Common::GeneralChannel *      outChannel,
				                                       const yarp::os::ConstString & nameAndPort) :
	inherited(), _viconClient(), _nameAndPort(nameAndPort), _outChannel(outChannel)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("outChannel = ", outChannel); //####
	OD_LOG_S1s("nameAndPort = ", nameAndPort); //####
    OD_LOG_EXIT_P(this); //####
} // KinectV2EventThread::KinectV2EventThread

ViconDataStreamEventThread::~ViconDataStreamEventThread(void)
{
    OD_LOG_OBJENTER(); //####
	if (_viconClient.IsConnected().Connected)
	{
		_viconClient.Disconnect();
	}
    OD_LOG_OBJEXIT(); //####
} // KinectV2EventThread::~KinectV2EventThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void ViconDataStreamEventThread::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = NULL;
    OD_LOG_OBJEXIT(); //####
} // ViconDataStreamEventThread::clearOutputChannel

bool ViconDataStreamEventThread::initializeConnection(void)
{
    OD_LOG_OBJENTER(); //####
	bool result = false;

	for (int ii = 0; (! result) && (kNumConnectTries > ii); ++ii)
	{
		if (_viconClient.IsConnected().Connected)
		{
			result = true;
		}
		else
		{
			_viconClient.Connect(_nameAndPort.c_str());
            Utilities::GoToSleep(kLittleSleep);
		}
    }
	if (result)
	{
		_viconClient.EnableMarkerData();
		_viconClient.EnableSegmentData();
#if defined(VICON_Y_UP)
        _viconClient.SetAxisMapping(CPP::Direction::Forward, CPP::Direction::Up,
                                    CPP::Direction::Right);
#elif defined(VICON_Z_UP)
        _viconClient.SetAxisMapping(CPP::Direction::Forward, CPP::Direction::Left,
                                    CPP::Direction::Up);
#endif // defined(VICON_Z_UP)
		_viconClient.SetStreamMode(VICON_STREAM_MODE);
	}
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // ViconDataStreamEventThread::initializeConnection

#if defined(REPORT_EVENT_COUNT_)
static long lEventCount = 0; //####
#endif // defined(REPORT_EVENT_COUNT_)

void ViconDataStreamEventThread::processEventData(const unsigned int subjectCount)
{
    OD_LOG_OBJENTER(); //####
	OD_LOG_L1("subjectCount = ", subjectCount); //####
	yarp::os::Bottle message;

	cerr << "got data" << endl; //!!!!
	for (unsigned int ii = 0; subjectCount > ii; ++ii)
	{
		CPP::Output_GetSubjectName o_gsubjn = _viconClient.GetSubjectName(ii);

		if (CPP::Result::Success == o_gsubjn.Result)
		{
			cerr << "got subject name" << endl; //!!!!
			yarp::os::Bottle & aList = message.addList();

			aList.addString(static_cast<std::string>(o_gsubjn.SubjectName).c_str());
			yarp::os::Property &        aDict = aList.addDict();
			CPP::Output_GetSegmentCount o_gsegc =
                                                _viconClient.GetSegmentCount(o_gsubjn.SubjectName);

			if (CPP::Result::Success == o_gsegc.Result)
			{
				cerr << "got segment count, " << o_gsegc.SegmentCount << endl; //!!!!
				for (unsigned int jj = 0, segCount = o_gsegc.SegmentCount; segCount > jj; ++jj)
				{
					CPP::Output_GetSegmentName o_gsegn =
                                            _viconClient.GetSegmentName(o_gsubjn.SubjectName, jj);

					if (CPP::Result::Success == o_gsegn.Result)
					{
						cerr << "got segment name" << endl; //!!!!
#if defined(USE_SEGMENT_LOCAL_DATA)
						CPP::Output_GetSegmentLocalTranslation        o_gseglt =
                                    _viconClient.GetSegmentLocalTranslation(o_gsubjn.SubjectName,
                                                                            o_gsegn.SegmentName);
						CPP::Output_GetSegmentLocalRotationQuaternion o_gseglrq =
                                _viconClient.GetSegmentLocalRotationQuaternion(o_gsubjn.SubjectName,
                                                                               o_gsegn.SegmentName);
#else // ! defined(USE_SEGMENT_LOCAL_DATA)
						CPP::Output_GetSegmentGlobalTranslation        o_gseggt =
                                    _viconClient.GetSegmentGlobalTranslation(o_gsubjn.SubjectName,
                                                                             o_gsegn.SegmentName);
						CPP::Output_GetSegmentGlobalRotationQuaternion o_gseggrq =
                            _viconClient.GetSegmentGlobalRotationQuaternion(o_gsubjn.SubjectName,
                                                                            o_gsegn.SegmentName);
#endif // ! defined(USE_SEGMENT_LOCAL_DATA)
						
#if defined(USE_SEGMENT_LOCAL_DATA)
						if ((CPP::Result::Success == o_gseglt.Result) &&
                            (CPP::Result::Success == o_gseglrq.Result))
						{
							if (! (o_gseglt.Occluded || o_gseglrq.Occluded))
							{
								yarp::os::Value    stuff;
								yarp::os::Bottle * stuffAsList = stuff.asList();

								if (stuffAsList)
								{
									stuffAsList->addDouble(o_gseglt.Translation[0]);
									stuffAsList->addDouble(o_gseglt.Translation[1]);
									stuffAsList->addDouble(o_gseglt.Translation[2]);
									stuffAsList->addDouble(o_gseglrq.Rotation[0]);
									stuffAsList->addDouble(o_gseglrq.Rotation[1]);
									stuffAsList->addDouble(o_gseglrq.Rotation[2]);
									stuffAsList->addDouble(o_gseglrq.Rotation[3]);
									aDict.put(static_cast<std::string>(o_gsegn.SegmentName).c_str(),
                                              stuff);
								}
							}
						}
#else // ! defined(USE_SEGMENT_LOCAL_DATA)
						if ((CPP::Result::Success == o_gseggt.Result) &&
                            (CPP::Result::Success == o_gseggrq.Result))
						{
							cerr << "got segment translation and rotation" << endl; //!!!!
							if (! (o_gseggt.Occluded || o_gseggrq.Occluded))
							{
								cerr << "got unoccluded data" << endl; //!!!!
								yarp::os::Value    stuff;
								yarp::os::Bottle * stuffAsList = stuff.asList();

								if (stuffAsList)
								{
									stuffAsList->addDouble(o_gseggt.Translation[0]);
									stuffAsList->addDouble(o_gseggt.Translation[1]);
									stuffAsList->addDouble(o_gseggt.Translation[2]);
									stuffAsList->addDouble(o_gseggrq.Rotation[0]);
									stuffAsList->addDouble(o_gseggrq.Rotation[1]);
									stuffAsList->addDouble(o_gseggrq.Rotation[2]);
									stuffAsList->addDouble(o_gseggrq.Rotation[3]);
									aDict.put(static_cast<std::string>(o_gsegn.SegmentName).c_str(),
                                              stuff);
								}
							}
						}
#endif // ! defined(USE_SEGMENT_LOCAL_DATA)
					}
				}
			}
		}
	}
	if (_outChannel)
	{
#if defined(REPORT_EVENT_COUNT_)
		++lEventCount; //####
		cerr << "sending " << lEventCount << endl; //####
#endif // defined(REPORT_EVENT_COUNT_)
		if (0 < message.size())
		{
			if (! _outChannel->write(message))
			{
				OD_LOG("(! _outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
				Stall();
#endif // defined(MpM_StallOnSendProblem)
			}
		}
	}
    OD_LOG_OBJEXIT(); //####
} // ViconDataStreamEventThread::processEventData

void ViconDataStreamEventThread::run(void)
{
    OD_LOG_OBJENTER(); //####
    for ( ; ! isStopping(); )
    {
		cerr << "checking for a frame" << endl; //!!!!
		if (CPP::Result::Success == _viconClient.GetFrame().Result)
		{
			CPP::Output_GetSubjectCount o_gsubjc = _viconClient.GetSubjectCount();

			if (CPP::Result::Success == o_gsubjc.Result)
			{
				processEventData(o_gsubjc.SubjectCount);
			}
		}
		else
		{
			Utilities::GoToSleep(kLittleSleep);
		}
        yarp::os::Time::yield();
    }
    OD_LOG_OBJEXIT(); //####
} // ViconDataStreamEventThread::run

bool ViconDataStreamEventThread::threadInit(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = initializeConnection();

    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // ViconDataStreamEventThread::threadInit

void ViconDataStreamEventThread::threadRelease(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // ViconDataStreamEventThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
