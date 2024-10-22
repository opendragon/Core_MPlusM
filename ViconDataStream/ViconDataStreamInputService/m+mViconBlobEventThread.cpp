//--------------------------------------------------------------------------------------------------
//
//  File:       m+mViconBlobEventThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a thread that generates output from Vicon data.
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
//  Created:    2015-06-24
//
//--------------------------------------------------------------------------------------------------

#include "m+mViconBlobEventThread.hpp"

#include <m+m/m+mUtilities.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

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

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::ViconBlob;
#if (! defined(MpM_BuildDummyServices))
using namespace ViconDataStreamSDK;
#endif // ! defined(MpM_BuildDummyServices)
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

//#define VICON_Y_UP_ /* */

#define VICON_Z_UP_ /* */

//#define USE_SEGMENT_LOCAL_DATA_ /* */

//#define VICON_STREAM_MODE_ CPP::StreamMode::ClientPullPreFetch /* */
#define VICON_STREAM_MODE_ CPP::StreamMode::ServerPush /* */

/*! @brief The number of times we attempt to connect to the Vicon device server. */
static const int kNumConnectTries = 17;

/*! @brief The amount of time in milliseconds to sleep between network operations that need to be
 retried. */
static const int kLittleSleep = 200;

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

ViconBlobEventThread::ViconBlobEventThread(Common::GeneralChannel * outChannel,
                                           const YarpString &       nameAndPort) :
    inherited(),
#if (! defined(MpM_BuildDummyServices))
    _viconClient(),
#endif // ! defined(MpM_BuildDummyServices)
    _nameAndPort(nameAndPort), _outChannel(outChannel)
{
    ODL_ENTER(); //####
    ODL_P1("outChannel = ", outChannel); //####
    ODL_S1s("nameAndPort = ", nameAndPort); //####
    ODL_EXIT_P(this); //####
} // ViconBlobEventThread::ViconBlobEventThread

ViconBlobEventThread::~ViconBlobEventThread(void)
{
    ODL_OBJENTER(); //####
#if (! defined(MpM_BuildDummyServices))
    if (_viconClient.IsConnected().Connected)
    {
        _viconClient.Disconnect();
    }
#endif // ! defined(MpM_BuildDummyServices)
    ODL_OBJEXIT(); //####
} // ViconBlobEventThread::~ViconBlobEventThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
ViconBlobEventThread::clearOutputChannel(void)
{
    ODL_OBJENTER(); //####
    _outChannel = NULL;
    ODL_OBJEXIT(); //####
} // ViconBlobEventThread::clearOutputChannel

bool
ViconBlobEventThread::initializeConnection(void)
{
    ODL_OBJENTER(); //####
#if defined(MpM_BuildDummyServices)
    bool result = true;
#else // ! defined(MpM_BuildDummyServices)
    bool result = false;
#endif // ! defined(MpM_BuildDummyServices)

#if (! defined(MpM_BuildDummyServices))
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
# if defined(VICON_Y_UP_)
        _viconClient.SetAxisMapping(CPP::Direction::Forward, CPP::Direction::Up,
                                    CPP::Direction::Right);
# elif defined(VICON_Z_UP_)
        _viconClient.SetAxisMapping(CPP::Direction::Forward, CPP::Direction::Left,
                                    CPP::Direction::Up);
# endif // defined(VICON_Z_UP_)
        _viconClient.SetStreamMode(VICON_STREAM_MODE_);
    }
#endif // ! defined(MpM_BuildDummyServices)
    ODL_OBJEXIT_B(result); //####
    return result;
} // ViconBlobEventThread::initializeConnection

void
ViconBlobEventThread::processEventData(const unsigned int subjectCount)
{
    ODL_OBJENTER(); //####
    ODL_I1("subjectCount = ", subjectCount); //####
#if (! defined(MpM_BuildDummyServices))
    if (0 < subjectCount)
    {
        bool              okSoFar = true;
# if (! defined(MpM_UseCustomStringBuffer))
        std::stringstream outBuffer;
# endif // ! defined(MpM_UseCustomStringBuffer)

# if defined(MpM_UseCustomStringBuffer)
        _outBuffer.reset().addLong(subjectCount).addString(LINE_END_);
# else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << subjectCount << LINE_END_;
# endif // ! defined(MpM_UseCustomStringBuffer)
        for (unsigned int ii = 0; okSoFar && (subjectCount > ii); ++ii)
        {
            CPP::Output_GetSubjectName o_gsubjn = _viconClient.GetSubjectName(ii);

            if (CPP::Result::Success == o_gsubjn.Result)
            {
                CPP::Output_GetSegmentCount o_gsegc =
                                                _viconClient.GetSegmentCount(o_gsubjn.SubjectName);

                if (CPP::Result::Success == o_gsegc.Result)
                {
                    const char * subjName = static_cast<std::string>(o_gsubjn.SubjectName).c_str();

# if defined(MpM_UseCustomStringBuffer)
                    _outBuffer.addString(subjName).addTab().addLong(o_gsegc.SegmentCount).
                        addString("\t0" LINE_END_);
# else // ! defined(MpM_UseCustomStringBuffer)
                    outBuffer << subjName << "\t" << o_gsegc.SegmentCount << "\t0" LINE_END_;
# endif // ! defined(MpM_UseCustomStringBuffer)
                    for (unsigned int jj = 0, segCount = o_gsegc.SegmentCount;
                         okSoFar && (segCount > jj); ++jj)
                    {
                        CPP::Output_GetSegmentName o_gsegn =
                                            _viconClient.GetSegmentName(o_gsubjn.SubjectName, jj);

                        if (CPP::Result::Success == o_gsegn.Result)
                        {
                            const char *                                  segName =
                                            static_cast<std::string>(o_gsegn.SegmentName).c_str();
# if defined(USE_SEGMENT_LOCAL_DATA_)
                            CPP::Output_GetSegmentLocalTranslation        o_gseglt =
                                    _viconClient.GetSegmentLocalTranslation(o_gsubjn.SubjectName,
                                                                            o_gsegn.SegmentName);
                            CPP::Output_GetSegmentLocalRotationQuaternion o_gseglrq =
                                _viconClient.GetSegmentLocalRotationQuaternion(o_gsubjn.SubjectName,
                                                                               o_gsegn.SegmentName);
# else // ! defined(USE_SEGMENT_LOCAL_DATA_)
                            CPP::Output_GetSegmentGlobalTranslation        o_gseggt =
                                    _viconClient.GetSegmentGlobalTranslation(o_gsubjn.SubjectName,
                                                                             o_gsegn.SegmentName);
                            CPP::Output_GetSegmentGlobalRotationQuaternion o_gseggrq =
                            _viconClient.GetSegmentGlobalRotationQuaternion(o_gsubjn.SubjectName,
                                                                            o_gsegn.SegmentName);
# endif // ! defined(USE_SEGMENT_LOCAL_DATA_)

# if defined(USE_SEGMENT_LOCAL_DATA_)
                            if ((CPP::Result::Success == o_gseglt.Result) &&
                                (CPP::Result::Success == o_gseglrq.Result))
                            {
                                if (! (o_gseglt.Occluded || o_gseglrq.Occluded))
                                {
#  if defined(MpM_UseCustomStringBuffer)
                                    _outBuffer.addString(segName).addTab();
                                    _outBuffer.addDouble(o_gseglt.Translation[0] * _scale).
                                        addTab();
                                    _outBuffer.addDouble(o_gseglt.Translation[1] * _scale).
                                        addTab();
                                    _outBuffer.addDouble(o_gseglt.Translation[2] * _scale).
                                        addTab();
                                    _outBuffer.addDouble(o_gseglrq.Rotation[0]).addTab();
                                    _outBuffer.addDouble(o_gseglrq.Rotation[1]).addTab();
                                    _outBuffer.addDouble(o_gseglrq.Rotation[2]).addTab();
                                    _outBuffer.addDouble(o_gseglrq.Rotation[3]).
                                        addString(LINE_END_);
#  else // ! defined(MpM_UseCustomStringBuffer)
                                    outBuffer << segName << "\t" <<
                                                (o_gseglt.Translation[0] * _scale) << "\t" <<
                                                (o_gseglt.Translation[1] * _scale) << "\t" <<
                                                (o_gseglt.Translation[2] * _scale) << "\t" <<
                                                o_gseglrq.Rotation[0] << "\t" <<
                                                o_gseglrq.Rotation[1] << "\t" <<
                                                o_gseglrq.Rotation[2] << "\t" <<
                                                o_gseglrq.Rotation[3] << LINE_END_;
#  endif // ! defined(MpM_UseCustomStringBuffer)
                                }
                            }
# else // ! defined(USE_SEGMENT_LOCAL_DATA_)
                            if ((CPP::Result::Success == o_gseggt.Result) &&
                                (CPP::Result::Success == o_gseggrq.Result))
                            {
                                if (! (o_gseggt.Occluded || o_gseggrq.Occluded))
                                {
#  if defined(MpM_UseCustomStringBuffer)
                                    _outBuffer.addString(segName).addTab();
                                    _outBuffer.addDouble(o_gseggt.Translation[0] * _scale).
                                        addTab();
                                    _outBuffer.addDouble(o_gseggt.Translation[1] * _scale).
                                        addTab();
                                    _outBuffer.addDouble(o_gseggt.Translation[2] * _scale).
                                        addTab();
                                    _outBuffer.addDouble(o_gseggrq.Rotation[0]).addTab();
                                    _outBuffer.addDouble(o_gseggrq.Rotation[1]).addTab();
                                    _outBuffer.addDouble(o_gseggrq.Rotation[2]).addTab();
                                    _outBuffer.addDouble(o_gseggrq.Rotation[3]).
                                        addString(LINE_END_);
#  else // ! defined(MpM_UseCustomStringBuffer)
                                    outBuffer << segName << "\t" <<
                                                (o_gseggt.Translation[0] * _scale) << "\t" <<
                                                (o_gseggt.Translation[1] * _scale) << "\t" <<
                                                (o_gseggt.Translation[2] * _scale) << "\t" <<
                                                o_gseggrq.Rotation[0] << "\t" <<
                                                o_gseggrq.Rotation[1] << "\t" <<
                                                o_gseggrq.Rotation[2] << "\t" <<
                                                o_gseggrq.Rotation[3] << LINE_END_;
#  endif // ! defined(MpM_UseCustomStringBuffer)
                                }
                            }
# endif // ! defined(USE_SEGMENT_LOCAL_DATA_)
                            else
                            {
                                okSoFar = false;
                            }
                        }
                        else
                        {
                            okSoFar = false;
                        }
                    }
                }
                else
                {
                    okSoFar = false;
                }
            }
            else
            {
                okSoFar = false;
            }
# if defined(MpM_UseCustomStringBuffer)
            _outBuffer.addString("END" LINE_END_);
# else // ! defined(MpM_UseCustomStringBuffer)
            outBuffer << "END" LINE_END_;
# endif // ! defined(MpM_UseCustomStringBuffer)
        }
        if (okSoFar && _outChannel)
        {
            const char * outString;
            size_t       outLength;
# if (! defined(MpM_UseCustomStringBuffer))
            std::string  buffAsString(outBuffer.str());
# endif // ! defined(MpM_UseCustomStringBuffer)

# if defined(MpM_UseCustomStringBuffer)
            outString = _outBuffer.getString(outLength);
# else // ! defined(MpM_UseCustomStringBuffer)
            outString = buffAsString.c_str();
            outLength = buffAsString.length();
# endif // ! defined(MpM_UseCustomStringBuffer)
            if (outString && outLength)
            {
                void *          rawString = static_cast<void *>(const_cast<char *>(outString));
                yarp::os::Value blobValue(rawString, static_cast<int>(outLength));

                _messageBottle.clear();
                _messageBottle.add(blobValue);
                if (! _outChannel->write(_messageBottle))
                {
                    ODL_LOG("(! _outChannel->write(_messageBottle))"); //####
# if defined(MpM_StallOnSendProblem)
                    Stall();
# endif // defined(MpM_StallOnSendProblem)
                }
            }
        }
    }
#endif // ! defined(MpM_BuildDummyServices)
    ODL_OBJEXIT(); //####
} // ViconBlobEventThread::processEventData

void
ViconBlobEventThread::run(void)
{
    ODL_OBJENTER(); //####
    for ( ; ! isStopping(); )
    {
#if (! defined(MpM_BuildDummyServices))
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
#endif // ! defined(MpM_BuildDummyServices)
        ConsumeSomeTime();
    }
    ODL_OBJEXIT(); //####
} // ViconBlobEventThread::run

void
ViconBlobEventThread::setScale(const double newScale)
{
    ODL_OBJENTER(); //####
    ODL_D1("newScale = ", newScale); //####
    _scale = newScale;
    ODL_OBJEXIT(); //####
} // ViconBlobEventThread::setScale

bool
ViconBlobEventThread::threadInit(void)
{
    ODL_OBJENTER(); //####
    bool result = initializeConnection();

    ODL_OBJEXIT_B(result); //####
    return result;
} // ViconBlobEventThread::threadInit

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
