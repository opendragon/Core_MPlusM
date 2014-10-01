//--------------------------------------------------------------------------------------------------
//
//  File:       M+MKinectV2EventThread.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for an output-generating thread for M+M.
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
//                  list of conditions and the following disclaimer in the documentation and/or
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
//  Created:    2014-09-26
//
//--------------------------------------------------------------------------------------------------

#include "M+MKinectV2EventThread.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for an output-generating thread for M+M. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::KinectV2;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

//#define REPORT_EVENT_COUNT /* */

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Add a three-dimensional floating-point vector to a dictionary.
 @param dictionary The dictionary to be updated.
 @param tag The tag to associate with the vector.
 @param position The vector to be added. */
static void add3VectorToDictionary(yarp::os::Property &          dictionary,
                                   const yarp::os::ConstString & tag,
                                   const CameraSpacePoint &      position)
{
    yarp::os::Value    stuff;
    yarp::os::Bottle * stuffAsList = stuff.asList();

    if (stuffAsList)
    {
        stuffAsList->addDouble(position.X);
        stuffAsList->addDouble(position.Y);
        stuffAsList->addDouble(position.Z);
        dictionary.put(tag, stuff);
    }
} // add3VectorToDictionary

/*! @brief Add a four-dimensional floating-point vector to a dictionary.
 @param dictionary The dictionary to be updated.
 @param tag The tag to associate with the vector.
 @param orientation The vector to be added. */
static void add4VectorToDictionary(yarp::os::Property &          dictionary,
                                   const yarp::os::ConstString & tag,
                                   const Vector4 &               orientation)
{
    yarp::os::Value    stuff;
    yarp::os::Bottle * stuffAsList = stuff.asList();

    if (stuffAsList)
    {
        stuffAsList->addDouble(orientation.x);
        stuffAsList->addDouble(orientation.y);
        stuffAsList->addDouble(orientation.z);
        stuffAsList->addDouble(orientation.w);
        dictionary.put(tag, stuff);
    }
} // add4VectorToDictionary

/*! @brief Add the description of a bone to a list.
 @param listToUpdate The list to be added to.
 @param jointTag The name of the bone.
 @param startJoint The beginning joint of the bone.
 @param endJoint The ending joint of the bone. */
static void addBoneToList(yarp::os::Bottle &            listToUpdate,
                          const yarp::os::ConstString & jointTag,
                          const Joint &                 startJoint,
                          const Joint &                 endJoint,
                          const JointOrientation &      startOrientation,
                          const JointOrientation &      endOrientation)
{
    // If we can't find either of these joints, exit
    if ((TrackingState_NotTracked != startJoint.TrackingState) &&
        (TrackingState_NotTracked != endJoint.TrackingState))
    {
        // Don't process if both points are inferred
        if ((TrackingState_Inferred != startJoint.TrackingState) ||
            (TrackingState_Inferred != endJoint.TrackingState))
        {
            yarp::os::Property & boneProps = listToUpdate.addDict();

            boneProps.put("tag", jointTag);
            add3VectorToDictionary(boneProps, "startposition", startJoint.Position);
            add3VectorToDictionary(boneProps, "endposition", endJoint.Position);
            add4VectorToDictionary(boneProps, "startorientation", startOrientation.Orientation);
            add4VectorToDictionary(boneProps, "endorientation", endOrientation.Orientation);
        }
    }
} // addBoneToList

/*! @brief Convert a hand state into a string.
 @param theHandState The state of the hand.
 @returns The state of the hand as a string. */
static const char * handStateToString(const HandState theHandState)
{
    const char * result;

    switch (theHandState)
    {
        case HandState_Closed :
            result = "closed";
            break;

        case HandState_Open :
            result = "open";
            break;

        case HandState_Lasso :
            result = "lasso";
            break;

        default :
            result = "unknown";
            break;

    }
    return result;
} // handStateToString

/*! @brief Convert a hand state confidence into a string.
 @param theHandConfidence The confidence in the state of the hand.
 @returns The confidence of the hand state as a string. */
static const char * handConfidenceToString(const TrackingConfidence theHandConfidence)
{
    const char * result;

    switch (theHandConfidence)
    {
        case TrackingConfidence_Low :
            result = "low";
            break;

        case TrackingConfidence_High :
            result = "high";
            break;

        default :
            result = "unknown";
            break;

    }
    return result;
} // handConfidenceToString

/*! @brief Add a bone to the list that's being built.
 @param str_ The name for the bone.
 @param start_ The starting joint for the bone.
 @param end_ The ending joint for the bone. */
#define ADD_BONE_TO_LIST_(str_, start_, end_) \
        addBoneToList(*bonesList, str_, joints[start_], joints[end_], \
                        jointOrientations[start_], jointOrientations[end_])

/*! @brief Add the data for a body to a message.
 @param message The message to be updated with the body data.
 @param joints The set of joints for the body.
 @param jointOrientations The orientations of the joints.
 @param leftHandState The state of the left hand.
 @param leftHandConfidence The confidence in the value of the state of the left hand.
 @param rightHandState The state of the right hand.
 @param rightHandConfidence The confidence in the value of the state of the right hand. */
static void addBodyToMessage(yarp::os::Bottle &       message,
                             const Joint *            joints,
                             const JointOrientation * jointOrientations,
                             const HandState          leftHandState,
                             const TrackingConfidence leftHandConfidence,
                             const HandState          rightHandState,
                             const TrackingConfidence rightHandConfidence)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("message = ", &message, "joints = ", joints, "jointOrientations = ", //####
              jointOrientations); //####
    yarp::os::Property & bodyProps = message.addDict();

    bodyProps.put("lefthand", handStateToString(leftHandState));
    bodyProps.put("righthand", handStateToString(rightHandState));
    bodyProps.put("lefthandconfidence", handConfidenceToString(leftHandConfidence));
    bodyProps.put("righthandconfidence", handConfidenceToString(rightHandConfidence));
    yarp::os::Value    bones;
    yarp::os::Bottle * bonesList = bones.asList();

    if (bonesList)
    {
        // Torso
        ADD_BONE_TO_LIST_("head2neck", JointType_Head, JointType_Neck);
        ADD_BONE_TO_LIST_("neck2spineshoulder", JointType_Neck, JointType_SpineShoulder);
        ADD_BONE_TO_LIST_("spineshoulder2spinemid", JointType_SpineShoulder, JointType_SpineMid);
        ADD_BONE_TO_LIST_("spinemid2spinebase", JointType_SpineMid, JointType_SpineBase);
        ADD_BONE_TO_LIST_("spineshoulder2shoulderright", JointType_SpineShoulder, JointType_ShoulderRight);
        ADD_BONE_TO_LIST_("spineshoulder2shoulderleft", JointType_SpineShoulder, JointType_ShoulderLeft);
        ADD_BONE_TO_LIST_("spinebase2hipright", JointType_SpineBase, JointType_HipRight);
        ADD_BONE_TO_LIST_("spinebase2hipleft", JointType_SpineBase, JointType_HipLeft);

        // Right arm
        ADD_BONE_TO_LIST_("shoulderright2elbowright", JointType_ShoulderRight,
                          JointType_ElbowRight);
        ADD_BONE_TO_LIST_("elbowright2wristright", JointType_ElbowRight, JointType_WristRight);
        ADD_BONE_TO_LIST_("wristright2handright", JointType_WristRight, JointType_HandRight);
        ADD_BONE_TO_LIST_("handright2handtipright", JointType_HandRight, JointType_HandTipRight);
        ADD_BONE_TO_LIST_("wristright2thumbright", JointType_WristRight, JointType_ThumbRight);

        // Left arm
        ADD_BONE_TO_LIST_("shoulderleft2elbowleft", JointType_ShoulderLeft, JointType_ElbowLeft);
        ADD_BONE_TO_LIST_("elbowleft2wristleft", JointType_ElbowLeft, JointType_WristLeft);
        ADD_BONE_TO_LIST_("wristleft2handleft", JointType_WristLeft, JointType_HandLeft);
        ADD_BONE_TO_LIST_("handleft2handtipleft", JointType_HandLeft, JointType_HandTipLeft);
        ADD_BONE_TO_LIST_("wristleft2thumbleft", JointType_WristLeft, JointType_ThumbLeft);

        // Right leg
        ADD_BONE_TO_LIST_("hipright2kneeright", JointType_HipRight, JointType_KneeRight);
        ADD_BONE_TO_LIST_("kneeright2ankleright", JointType_KneeRight, JointType_AnkleRight);
        ADD_BONE_TO_LIST_("ankleright2footright", JointType_AnkleRight, JointType_FootRight);

        // Left leg
        ADD_BONE_TO_LIST_("hipleft2kneeleft", JointType_HipLeft, JointType_KneeLeft);
        ADD_BONE_TO_LIST_("kneeleft2ankleleft", JointType_KneeLeft, JointType_AnkleLeft);
        ADD_BONE_TO_LIST_("ankleleft2footleft", JointType_AnkleLeft, JointType_FootLeft);

        // Add them all
        bodyProps.put("bones", bones);
    }
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
                Joint              joints[JointType_Count];
                JointOrientation   jointOrientations[JointType_Count];
                HandState          leftHandState = HandState_Unknown;
                HandState          rightHandState = HandState_Unknown;
                TrackingConfidence leftHandConfidence = TrackingConfidence_Low;
                TrackingConfidence rightHandConfidence = TrackingConfidence_Low;

                pBody->get_HandLeftState(&leftHandState);
                pBody->get_HandRightState(&rightHandState);
                pBody->get_HandLeftConfidence(&leftHandConfidence);
                pBody->get_HandRightConfidence(&rightHandConfidence);
                hr = pBody->GetJoints(_countof(joints), joints);
                if (SUCCEEDED(hr))
                {
                    hr = pBody->GetJointOrientations(_countof(jointOrientations),
                                                     jointOrientations);
                }
                if (SUCCEEDED(hr))
                {
                    addBodyToMessage(message, joints, jointOrientations, leftHandState,
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

KinectV2EventThread::KinectV2EventThread(Common::GeneralChannel * outChannel) :
    inherited(), _kinectSensor(NULL), _bodyFrameReader(NULL), _bodyFrameSource(NULL),
    _outChannel(outChannel)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("outChannel = ", outChannel); //####
    OD_LOG_EXIT_P(this); //####
} // KinectV2EventThread::KinectV2EventThread

KinectV2EventThread::~KinectV2EventThread(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // KinectV2EventThread::~KinectV2EventThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void KinectV2EventThread::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = NULL;
    OD_LOG_OBJEXIT(); //####
} // KinectV2EventThread::clearOutputChannel

HRESULT KinectV2EventThread::initializeDefaultSensor(void)
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
            std::cerr << "problem?!?!" << std::endl;
            //SetStatusMessage(L"No ready Kinect found!", 10000, true);
            hr = E_FAIL;
        }
    }
    OD_LOG_OBJEXIT_L(hr); //####
    return hr;
} // KinectV2EventThread::initializeDefaultSensor

#if defined(REPORT_EVENT_COUNT)
static long lEventCount = 0; //####
#endif // defined(REPORT_EVENT_COUNT)

void KinectV2EventThread::processEventData(void)
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
#if defined(REPORT_EVENT_COUNT)
                        ++lEventCount; //####
                        std::cerr << "sending " << lEventCount << std::endl; //####
#endif // defined(REPORT_EVENT_COUNT)
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
} // KinectV2EventThread::processEventData

void KinectV2EventThread::run(void)
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
} // KinectV2EventThread::run

bool KinectV2EventThread::threadInit(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = SUCCEEDED(initializeDefaultSensor());

    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // KinectV2EventThread::threadInit

void KinectV2EventThread::threadRelease(void)
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
} // KinectV2EventThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
