//--------------------------------------------------------------------------------------------------
//
//  File:       m+mKinectV2BlobEventThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a thread that generates output from Kinect V2 data.
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
//  Created:    2015-07-26
//
//--------------------------------------------------------------------------------------------------

#include "m+mKinectV2BlobEventThread.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a thread that generates output from Kinect V2 data. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::KinectV2Blob;
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

/*! @brief Add the description of a joint to the output.
 @param outBuffer The output buffer to be updated with the body data.
 @param jointTag The name of the bone.
 @param jointData The joint position.
 @param orientationData The orientation of the joint. */
#if defined(MpM_UseCustomStringBuffer)
static void addJointToBuffer(const YarpString &       jointTag,
                             const Joint &            jointData,
                             const JointOrientation & orientationData)
#else // ! defined(MpM_UseCustomStringBuffer)
static void addJointToBuffer(std::stringstream &      outBuffer,
                             const YarpString &       jointTag,
                             const Joint &            jointData,
                             const JointOrientation & orientationData)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    // If we can't find either of these joints, exit
    if (TrackingState_NotTracked != jointData.TrackingState)
    {
        // Don't process if point is inferred
        if (TrackingState_Inferred != jointData.TrackingState)
        {
#if defined(MpM_UseCustomStringBuffer)
            _outBuffer.addString(jointTag).addTab();
            _outBuffer.addDouble(jointData.Position.X * _translationScale).addTab();
            _outBuffer.addDouble(jointData.Position.Y * _translationScale).addTab();
            _outBuffer.addDouble(jointData.Position.Z * _translationScale).addTab();
            _outBuffer.addDouble(orientationData.Orientation.x).addTab();
            _outBuffer.addDouble(orientationData.Orientation.y).addTab();
            _outBuffer.addDouble(orientationData.Orientation.z).addTab();
            _outBuffer.addDouble(orientationData.Orientation.w).addString(LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
            outBuffer << jointTag << "\t" << (jointData.Position.X * _translationScale) << "\t" <<
                        (jointData.Position.Y * _translationScale) << "\t" <<
                        (jointData.Position.Z * _translationScale) << "\t" <<
                        orientationData.Orientation.x << "\t" << orientationData.Orientation.y <<
                        orientationData.Orientation.z << "\t" << orientationData.Orientation.w <<
                        LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
        }
    }
} // addJointToBuffer

/*! @brief Add a joint to the output that's being built.
 @param str_ The name for the joint.
 @param index_ The joint index. */
#if defined(MpM_UseCustomStringBuffer)
# define ADD_JOINT_TO_BUFFER_(str_, index_) \
    addJointToBuffer(str_, jointData[index_], orientationData[index_])
#else // ! defined(MpM_UseCustomStringBuffer)
# define ADD_JOINT_TO_BUFFER_(str_, index_) \
    addJointToBuffer(outBuffer, str_, jointData[index_], orientationData[index_])
#endif // ! defined(MpM_UseCustomStringBuffer)

/*! @brief Add the data for a body to a message.
 @param outBuffer The output buffer to be updated with the body data.
 @param jointData The set of joints for the body.
 @param orientationData The orientations of the joints. */
#if defined(MpM_UseCustomStringBuffer)
static void addBodyToMessage(const Joint *            jointData,
                             const JointOrientation * orientationData)
#else // ! defined(MpM_UseCustomStringBuffer)
static void addBodyToMessage(std::stringstream &      outBuffer,
                             const Joint *            jointData,
                             const JointOrientation * orientationData)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    OD_LOG_ENTER(); //####
#if defined(MpM_UseCustomStringBuffer)
    OD_LOG_P2("jointData = ", jointData, "orientationData = ", orientationData); //####
#else // ! defined(MpM_UseCustomStringBuffer)
    OD_LOG_P3("outBuffer = ", &outBuffer, "jointData = ", jointData, "orientationData = ", //####
              orientationData); //####
#endif // ! defined(MpM_UseCustomStringBuffer)
    // Torso
    ADD_JOINT_TO_BUFFER_("head", JointType_Head);
    ADD_JOINT_TO_BUFFER_("neck", JointType_Neck);
    ADD_JOINT_TO_BUFFER_("spineshoulder", JointType_SpineShoulder);
    ADD_JOINT_TO_BUFFER_("spinemid", JointType_SpineMid);
    ADD_JOINT_TO_BUFFER_("spinebase", JointType_SpineBase);
    ADD_JOINT_TO_BUFFER_("shoulderright", JointType_ShoulderRight);
    ADD_JOINT_TO_BUFFER_("shoulderleft", JointType_ShoulderLeft);
    ADD_JOINT_TO_BUFFER_("hipright", JointType_HipRight);
    ADD_JOINT_TO_BUFFER_("hipleft", JointType_HipLeft);

    // Right arm
    ADD_JOINT_TO_BUFFER_("elbowright", JointType_ElbowRight);
    ADD_JOINT_TO_BUFFER_("wristright", JointType_WristRight);
    ADD_JOINT_TO_BUFFER_("handright", JointType_HandRight);
    ADD_JOINT_TO_BUFFER_("handtipright", JointType_HandTipRight);
    ADD_JOINT_TO_BUFFER_("thumbright", JointType_ThumbRight);
   
    // Left arm
    ADD_JOINT_TO_BUFFER_("elbowleft", JointType_ElbowLeft);
    ADD_JOINT_TO_BUFFER_("wristleft", JointType_WristLeft);
    ADD_JOINT_TO_BUFFER_("handleft", JointType_HandLeft);
    ADD_JOINT_TO_BUFFER_("handtipleft", JointType_HandTipLeft);
    ADD_JOINT_TO_BUFFER_("thumbleft", JointType_ThumbLeft);
    
    // Right leg
    ADD_JOINT_TO_BUFFER_("kneeright", JointType_KneeRight);
    ADD_JOINT_TO_BUFFER_("ankleright", JointType_AnkleRight);
    ADD_JOINT_TO_BUFFER_("footright", JointType_FootRight);

    // Left leg
    ADD_JOINT_TO_BUFFER_("kneeleft", JointType_KneeLeft);
    ADD_JOINT_TO_BUFFER_("ankleleft", JointType_AnkleLeft);
    ADD_JOINT_TO_BUFFER_("footleft", JointType_FootLeft);

    OD_LOG_EXIT(); //####
} // addBodyToMessage

/*! @brief Process the data returned by the Kinect V2 sensor.
 @param nBodyCount The number of 'bodies' in the sensor data.
 @param ppBodies The sensor data.
 @returns @c true if at least one body was added to the message successfully, and @c false
 otherwise. */
static bool processBody(const int nBodyCount,
                        IBody * * ppBodies)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("message = ", message, "ppBodies = ", ppBodies); //####
    OD_LOG_L1("nBodyCount = ", nBodyCount); //####
#if (! defined(MpM_UseCustomStringBuffer))
    std::stringstream outBuffer;
#endif // ! defined(MpM_UseCustomStringBuffer)
    bool              result = false;
    int               actualBodyCount = 0;

    for (int ii = 0; nBodyCount > ii; ++ii)
    {
        IBody * pBody = ppBodies[ii];

        if (pBody)
        {
            BOOLEAN bTracked = false;
            HRESULT hr = pBody->get_IsTracked(&bTracked);

            if (SUCCEEDED(hr) && bTracked)
            {
                Joint jointData[JointType_Count];

                hr = pBody->GetJoints(_countof(jointData), jointData);
                if (SUCCEEDED(hr))
                {
                    JointOrientation orientationData[JointType_Count];

                    hr = pBody->GetJointOrientations(_countof(orientationData), orientationData);
                }
                if (SUCCEEDED(hr))
                {
                    ++actualBodyCount;
                }
            }
        }
    }
#if defined(MpM_UseCustomStringBuffer)
    _outBuffer.reset().addLong(actualBodyCount).addString(LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << actualBodyCount << LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
    for (int ii = 0; nBodyCount > ii; ++ii)
    {
        IBody * pBody = ppBodies[ii];

        if (pBody)
        {
            BOOLEAN bTracked = false;
            HRESULT hr = pBody->get_IsTracked(&bTracked);

            if (SUCCEEDED(hr) && bTracked)
            {
                Joint            jointData[JointType_Count];
                JointOrientation orientationData[JointType_Count];

                hr = pBody->GetJoints(_countof(jointData), jointData);
                if (SUCCEEDED(hr))
                {
                    hr = pBody->GetJointOrientations(_countof(orientationData), orientationData);
                }
                if (SUCCEEDED(hr))
                {
                    int actualJointCount = 0;

                    for (int jj = 0; JointType_Count > jj; ++jj)
                    {
                        // If we can't find either of these joints, exit
                        if (TrackingState_NotTracked != jointData[jj].TrackingState)
                        {
                            // Don't process if point is inferred
                            if (TrackingState_Inferred != jointData[jj].TrackingState)
                            {
                                ++actualJointCount;
                            }
                        }
                    }
#if defined(MpM_UseCustomStringBuffer)
                    _outBuffer.addLong(static_cast<int>(ii)).addTab().
                        addLong(actualJointCount).addString(LINE_END_);
                    addBodyToMessage(jointData, orientationData);
#else // ! defined(MpM_UseCustomStringBuffer)
                    outBuffer << ii << "\t" << actualJointCount << LINE_END_;
                    addBodyToMessage(outBuffer, jointData, orientationData);
#endif // ! defined(MpM_UseCustomStringBuffer)
                    result = true;
                }
            }
        }
    }
#if defined(MpM_UseCustomStringBuffer)
    _outBuffer.addString("END" LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << "END" LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
    const char * outString;
    size_t       outLength;
#if (! defined(MpM_UseCustomStringBuffer))
    std::string  buffAsString(outBuffer.str());
#endif // ! defined(MpM_UseCustomStringBuffer)

#if defined(MpM_UseCustomStringBuffer)
    outString = _outBuffer.getString(outLength);
#else // ! defined(MpM_UseCustomStringBuffer)
    outString = bufAsString.c_str();
    outLength = bufAsString.length();
#endif // ! defined(MpM_UseCustomStringBuffer)
    if (outString && outLength)
    {
        void *          rawString = static_cast<void *>(const_cast<char *>(outString));
        yarp::os::Value blobValue(rawString, static_cast<int>(outLength));

        _messageBottle.clear();
        _messageBottle.add(blobValue);
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // processBody

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

KinectV2BlobEventThread::KinectV2BlobEventThread(Common::GeneralChannel * outChannel) :
    inherited(), _translationScale(1), _kinectSensor(NULL), _bodyFrameReader(NULL),
    _bodyFrameSource(NULL), _outChannel(outChannel)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("outChannel = ", outChannel); //####
    OD_LOG_EXIT_P(this); //####
} // KinectV2BlobEventThread::KinectV2BlobEventThread

KinectV2BlobEventThread::~KinectV2BlobEventThread(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // KinectV2BlobEventThread::~KinectV2BlobEventThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void KinectV2BlobEventThread::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = NULL;
    OD_LOG_OBJEXIT(); //####
} // KinectV2BlobEventThread::clearOutputChannel

HRESULT KinectV2BlobEventThread::initializeDefaultSensor(void)
{
    OD_LOG_OBJENTER(); //####
    HRESULT hr = GetDefaultKinectSensor(&_kinectSensor);

    if (! FAILED(hr))
    {
        if (_kinectSensor)
        {
            // Initialize the Kinect and get the body reader
            hr = _kinectSensor->Open();
            if (SUCCEEDED(hr))
            {
                hr = _kinectSensor->get_BodyFrameSource(&_bodyFrameSource);
            }
            if (SUCCEEDED(hr))
            {
                hr = _bodyFrameSource->OpenReader(&_bodyFrameReader);
            }
            if (SUCCEEDED(hr))
            {
                hr = _bodyFrameReader->SubscribeFrameArrived(&_frameEventHandle);
            }
        }
        if ((! _kinectSensor) || FAILED(hr))
        {
            cerr << "problem?!?!" << endl;
            //SetStatusMessage(L"No ready Kinect found!", 10000, true);
            hr = E_FAIL;
        }
    }
    OD_LOG_OBJEXIT_L(hr); //####
    return hr;
} // KinectV2BlobEventThread::initializeDefaultSensor

void KinectV2BlobEventThread::processEventData(void)
{
    OD_LOG_OBJENTER(); //####
    if (_bodyFrameReader)
    {
        IBodyFrameArrivedEventArgs * eventData = NULL;
        HRESULT                      hr =
                                    _bodyFrameReader->GetFrameArrivedEventData(_frameEventHandle,
                                                                               &eventData);

        if (SUCCEEDED(hr))
        {
            IBodyFrameReference * frameRef = NULL;

            hr = eventData->get_FrameReference(&frameRef);
            if (SUCCEEDED(hr))
            {
                IBodyFrame * bodyFrame = NULL;

                hr = frameRef->AcquireFrame(&bodyFrame);
                if (SUCCEEDED(hr))
                {
                    IBody * ppBodies[BODY_COUNT] = { NULL };

                    hr = bodyFrame->GetAndRefreshBodyData(_countof(ppBodies), ppBodies);
                    if (SUCCEEDED(hr))
                    {
                        _messageBottle.clear();
                        if (processBody(BODY_COUNT, ppBodies))
                        {

                        }
                        else
                        {
                            hr = S_FALSE;
                        }
                    }
                    for (int ii = 0; _countof(ppBodies) > ii; ++ii)
                    {
                        SafeRelease(ppBodies[ii]);
                    }
                    SafeRelease(bodyFrame);
                    if (SUCCEEDED(hr) && _outChannel)
                    {
                        if (0 < _messageBottle.size())
                        {
                            if (! _outChannel->write(_messageBottle))
                            {
                                OD_LOG("(! _outChannel->write(_messageBottle))"); //####
#if defined(MpM_StallOnSendProblem)
                                Stall();
#endif // defined(MpM_StallOnSendProblem)
                            }
                        }
                    }
                }
            }
        }
    }
    OD_LOG_OBJEXIT(); //####
} // KinectV2BlobEventThread::processEventData

void KinectV2BlobEventThread::run(void)
{
    OD_LOG_OBJENTER(); //####
    for ( ; ! isStopping(); )
    {
        MSG msg;

        while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
        {
            DispatchMessage(&msg);
        }
        if (_frameEventHandle)
        {
            HANDLE handles[] = { reinterpret_cast<HANDLE>(_frameEventHandle) };

            switch (MsgWaitForMultipleObjects(_countof(handles), handles, false, 1000, QS_ALLINPUT))
            {
                case WAIT_OBJECT_0 :
                    processEventData();
                    break;

                default :
                    break;
                    
            }
        }
        if (WM_QUIT == msg.message)
        {
            stop();
        }
        yarp::os::Time::yield();
    }
    OD_LOG_OBJEXIT(); //####
} // KinectV2BlobEventThread::run

void KinectV2BlobEventThread::setScale(const double newScale)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_D1("newScale = ", newScale); //####
    _translationScale = newScale;
    OD_LOG_OBJEXIT(); //####
} // KinectV2BlobEventThread::setScale

bool KinectV2BlobEventThread::threadInit(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = SUCCEEDED(initializeDefaultSensor());

    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // KinectV2BlobEventThread::threadInit

void KinectV2BlobEventThread::threadRelease(void)
{
    OD_LOG_OBJENTER(); //####
    if (_bodyFrameReader && _frameEventHandle)
    {
        _bodyFrameReader->UnsubscribeFrameArrived(_frameEventHandle);
    }
    _frameEventHandle = NULL;
    // done with body frame reader
    SafeRelease(_bodyFrameReader);
    SafeRelease(_bodyFrameSource);
    // close the Kinect Sensor
    if (_kinectSensor)
    {
        _kinectSensor->Close();
    }
    SafeRelease(_kinectSensor);
    OD_LOG_OBJEXIT(); //####
} // KinectV2BlobEventThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
