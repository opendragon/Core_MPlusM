//--------------------------------------------------------------------------------------------------
//
//  File:       m+mKinectV2SpecialEventThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a thread that generates output from Kinect V2 data.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by Simon Fraser University.
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
//  Created:    2015-10-30
//
//--------------------------------------------------------------------------------------------------

#include "m+mKinectV2SpecialEventThread.h"

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
using namespace MplusM::KinectV2Special;
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

/*! @brief Add a three-dimensional floating-point vector to a list.
 @param listToUpdate The list to be added to.
 @param position The vector to be added. */
static void add3VectorToList(yarp::os::Bottle &       listToUpdate,
                             const CameraSpacePoint & position)
{
    listToUpdate.addDouble(position.X);
    listToUpdate.addDouble(position.Y);
    listToUpdate.addDouble(position.Z);
} // add3VectorToList

/*! @brief Add a three-dimensional floating-point zero vector to a list.
 @param listToUpdate The list to be added to. */
static void add3ZeroesToList(yarp::os::Bottle & listToUpdate)
{
    listToUpdate.addDouble(0);
    listToUpdate.addDouble(0);
    listToUpdate.addDouble(0);
} // add3ZeroesToList

/*! @brief Add a four-dimensional floating-point vector to a list.
 @param listToUpdate The list to be added to.
 @param orientation The vector to be added. */
static void add4VectorToList(yarp::os::Bottle & listToUpdate,
                             const Vector4 &    orientation)
{
    listToUpdate.addDouble(orientation.x);
    listToUpdate.addDouble(orientation.y);
    listToUpdate.addDouble(orientation.z);
    listToUpdate.addDouble(orientation.w);
} // add4VectorToList

/*! @brief Add a four-dimensional floating-point zero vector to a list.
 @param listToUpdate The list to be added to. */
static void add4ZeroesToList(yarp::os::Bottle & listToUpdate)
{
    listToUpdate.addDouble(0;
    listToUpdate.addDouble(0);
    listToUpdate.addDouble(0);
    listToUpdate.addDouble(0);
} // add4ZeroesToList

/*! @brief Add the description of a joint to a list.
 @param listToUpdate The list to be added to.
 @param jointData The joint position.
 @param orientationData The orientation of the joint. */
static void addJointToList(yarp::os::Bottle &       listToUpdate,
                           const Joint &            jointData,
                           const JointOrientation & orientationData)
{
    // If we can't find either of these joints, exit
    if (TrackingState_NotTracked == jointData.TrackingState)
    {
        listToUpdate.addInt(0);
        add3ZeroesToList(listToUpdate);
        add4ZeroesToList(listToUpdate);
    }
    else
    {
        // Don't process if point is inferred
        if (TrackingState_Inferred == jointData.TrackingState)
        {
            listToUpdate.addInt(0);
            add3ZeroesToList(listToUpdate);
            add4ZeroesToList(listToUpdate);
        }
        else
        {
            listToUpdate.addInt(1);
            add3VectorToList(listToUpdate, jointData.Position);
            add4VectorToList(listToUpdate, orientationData.Orientation);
        }
    }
} // addJointToList

/*! @brief Add a joint to the list that's being built.
 @param str_ The name for the joint.
 @param index_ The joint index. */
#define ADD_JOINT_TO_LIST_(index_) \
        addJointToList(*jointsList, jointData[index_], orientationData[index_])

/*! @brief Add the data for a body to a message.
 @param message The message to be updated with the body data.
 @param jointData The set of joints for the body.
 @param orientationData The orientations of the joints.
 @param leftHandState The state of the left hand.
 @param leftHandConfidence The confidence in the value of the state of the left hand.
 @param rightHandState The state of the right hand.
 @param rightHandConfidence The confidence in the value of the state of the right hand. */
static void addBodyToMessage(yarp::os::Bottle &       message,
                             const int                index,
                             const Joint *            jointData,
                             const JointOrientation * orientationData,
                             const HandState          leftHandState,
                             const TrackingConfidence leftHandConfidence,
                             const HandState          rightHandState,
                             const TrackingConfidence rightHandConfidence)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("message = ", &message, "jointData = ", jointData, "orientationData = ", //####
              orientationData); //####
    message.addInt(index);
    message.addInt(static_cast<int>(leftHandState));
    message.addInt(static_cast<int>(rightHandState));
    message.addInt(static_cast<int>(leftHandConfidence));
    message.addInt(static_cast<int>(rightHandConfidence));
    // Torso
    ADD_JOINT_TO_LIST_(JointType_Head);
    ADD_JOINT_TO_LIST_(JointType_Neck);
    ADD_JOINT_TO_LIST_(JointType_SpineShoulder);
    ADD_JOINT_TO_LIST_(JointType_SpineMid);
    ADD_JOINT_TO_LIST_(JointType_SpineBase);
    ADD_JOINT_TO_LIST_(JointType_ShoulderRight);
    ADD_JOINT_TO_LIST_(JointType_ShoulderLeft);
    ADD_JOINT_TO_LIST_(JointType_HipRight);
    ADD_JOINT_TO_LIST_(JointType_HipLeft);
    
    // Right arm
    ADD_JOINT_TO_LIST_(JointType_ElbowRight);
    ADD_JOINT_TO_LIST_(JointType_WristRight);
    ADD_JOINT_TO_LIST_(JointType_HandRight);
    ADD_JOINT_TO_LIST_(JointType_HandTipRight);
    ADD_JOINT_TO_LIST_(JointType_ThumbRight);
    
    // Left arm
    ADD_JOINT_TO_LIST_(JointType_ElbowLeft);
    ADD_JOINT_TO_LIST_(JointType_WristLeft);
    ADD_JOINT_TO_LIST_(JointType_HandLeft);
    ADD_JOINT_TO_LIST_(JointType_HandTipLeft);
    ADD_JOINT_TO_LIST_(JointType_ThumbLeft);
    
    // Right leg
    ADD_JOINT_TO_LIST_(JointType_KneeRight);
    ADD_JOINT_TO_LIST_(JointType_AnkleRight);
    ADD_JOINT_TO_LIST_(JointType_FootRight);
    
    // Left leg
    ADD_JOINT_TO_LIST_(JointType_KneeLeft);
    ADD_JOINT_TO_LIST_(JointType_AnkleLeft);
    ADD_JOINT_TO_LIST_(JointType_FootLeft);
    
    OD_LOG_EXIT(); //####
} // addBodyToMessage

/*! @brief Process the data returned by the Kinect V2 sensor.
 @param message The message to be updated with the sensor data.
 @param nBodyCount The number of 'bodies' in the sensor data.
 @param ppBodies The sensor data.
 @returns @c true if at least one body was added to the message successfully, and @c false
 otherwise. */
static bool processBody(yarp::os::Bottle & message,
                        const int          nBodyCount,
                        IBody * *          ppBodies)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("message = ", message, "ppBodies = ", ppBodies); //####
    OD_LOG_L1("nBodyCount = ", nBodyCount); //####
    bool result = false;

    for (int ii = 0; nBodyCount > ii; ++ii)
    {
        IBody * pBody = ppBodies[ii];

        if (pBody)
        {
            BOOLEAN bTracked = false;
            HRESULT hr = pBody->get_IsTracked(&bTracked);

            if (SUCCEEDED(hr) && bTracked)
            {
                Joint              jointData[JointType_Count];
                JointOrientation   orientationData[JointType_Count];
                HandState          leftHandState = HandState_Unknown;
                HandState          rightHandState = HandState_Unknown;
                TrackingConfidence leftHandConfidence = TrackingConfidence_Low;
                TrackingConfidence rightHandConfidence = TrackingConfidence_Low;

                pBody->get_HandLeftState(&leftHandState);
                pBody->get_HandRightState(&rightHandState);
                pBody->get_HandLeftConfidence(&leftHandConfidence);
                pBody->get_HandRightConfidence(&rightHandConfidence);
                hr = pBody->GetJoints(_countof(jointData), jointData);
                if (SUCCEEDED(hr))
                {
                    hr = pBody->GetJointOrientations(_countof(orientationData), orientationData);
                }
                if (SUCCEEDED(hr))
                {
                    addBodyToMessage(message, ii, jointData, orientationData, leftHandState,
                                     leftHandConfidence, rightHandState, rightHandConfidence);
                    result = true;
                }
            }
        }
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

KinectV2SpecialEventThread::KinectV2SpecialEventThread(Common::GeneralChannel * outChannel) :
    inherited(), _kinectSensor(NULL), _bodyFrameReader(NULL), _bodyFrameSource(NULL),
    _outChannel(outChannel)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("outChannel = ", outChannel); //####
    OD_LOG_EXIT_P(this); //####
} // KinectV2SpecialEventThread::KinectV2SpecialEventThread

KinectV2SpecialEventThread::~KinectV2SpecialEventThread(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // KinectV2SpecialEventThread::~KinectV2SpecialEventThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void KinectV2SpecialEventThread::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = NULL;
    OD_LOG_OBJEXIT(); //####
} // KinectV2SpecialEventThread::clearOutputChannel

HRESULT KinectV2SpecialEventThread::initializeDefaultSensor(void)
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
} // KinectV2SpecialEventThread::initializeDefaultSensor

void KinectV2SpecialEventThread::processEventData(void)
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
                    yarp::os::Bottle message;
                    IBody *          ppBodies[BODY_COUNT] = { NULL };

                    hr = bodyFrame->GetAndRefreshBodyData(_countof(ppBodies), ppBodies);
                    if (SUCCEEDED(hr))
                    {
                        if (! processBody(message, BODY_COUNT, ppBodies))
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
                }
            }
        }
    }
    OD_LOG_OBJEXIT(); //####
} // KinectV2SpecialEventThread::processEventData

void KinectV2SpecialEventThread::run(void)
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
} // KinectV2SpecialEventThread::run

bool KinectV2SpecialEventThread::threadInit(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = SUCCEEDED(initializeDefaultSensor());

    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // KinectV2SpecialEventThread::threadInit

void KinectV2SpecialEventThread::threadRelease(void)
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
} // KinectV2SpecialEventThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
