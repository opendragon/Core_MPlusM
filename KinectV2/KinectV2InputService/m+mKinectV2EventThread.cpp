//--------------------------------------------------------------------------------------------------
//
//  File:       m+mKinectV2EventThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a thread that generates output from Kinect V2 data.
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
//  Created:    2014-09-26
//
//--------------------------------------------------------------------------------------------------

#include "m+mKinectV2EventThread.h"

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
using namespace MplusM::KinectV2;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

//#define GENERATE_BONES_ /* */

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if (! defined(MpM_BuildDummyServices))
/*! @brief Add a three-dimensional floating-point vector to a dictionary.
 @param dictionary The dictionary to be updated.
 @param tag The tag to associate with the vector.
 @param position The vector to be added. */
static void
add3VectorToDictionary(yarp::os::Property &     dictionary,
                       const YarpString &       tag,
                       const CameraSpacePoint & position)
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
#endif // ! defined(MpM_BuildDummyServices)

#if (! defined(MpM_BuildDummyServices))
/*! @brief Add a four-dimensional floating-point vector to a dictionary.
 @param dictionary The dictionary to be updated.
 @param tag The tag to associate with the vector.
 @param orientation The vector to be added. */
static void
add4VectorToDictionary(yarp::os::Property & dictionary,
                       const YarpString &   tag,
                       const Vector4 &      orientation)
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
#endif // ! defined(MpM_BuildDummyServices)

#if (! defined(MpM_BuildDummyServices))
# if defined(GENERATE_BONES_)
/*! @brief Add the description of a bone to a list.
 @param listToUpdate The list to be added to.
 @param jointTag The name of the bone.
 @param startJoint The beginning joint of the bone.
 @param endJoint The ending joint of the bone.
 @param startOrientation The orientation of the beginning joint of the bone.
 @param endOrientation The orientation of the ending joint of the bone. */
static void
addBoneToList(yarp::os::Bottle &       listToUpdate,
              const YarpString &       jointTag,
              const Joint &            startJoint,
              const Joint &            endJoint,
              const JointOrientation & startOrientation,
              const JointOrientation & endOrientation)
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
# else // ! defined(GENERATE_BONES_)
/*! @brief Add the description of a joint to a list.
 @param listToUpdate The list to be added to.
 @param jointTag The name of the bone.
 @param jointData The joint position.
 @param orientationData The orientation of the joint. */
static void
addJointToList(yarp::os::Bottle &       listToUpdate,
               const YarpString &       jointTag,
               const Joint &            jointData,
               const JointOrientation & orientationData)
{
    // If we can't find either of these joints, exit
    if (TrackingState_NotTracked != jointData.TrackingState)
    {
        // Don't process if point is inferred
        if (TrackingState_Inferred != jointData.TrackingState)
        {
            yarp::os::Property & jointProps = listToUpdate.addDict();
            
            jointProps.put("tag", jointTag);
            add3VectorToDictionary(jointProps, "position", jointData.Position);
            add4VectorToDictionary(jointProps, "orientation", orientationData.Orientation);
        }
    }
} // addJointToList
# endif // ! defined(GENERATE_BONES_)
#endif // ! defined(MpM_BuildDummyServices)

#if (! defined(MpM_BuildDummyServices))
/*! @brief Convert a hand state into a string.
 @param theHandState The state of the hand.
 @returns The state of the hand as a string. */
static const char *
handStateToString(const HandState theHandState)
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
#endif // ! defined(MpM_BuildDummyServices)

#if (! defined(MpM_BuildDummyServices))
/*! @brief Convert a hand state confidence into a string.
 @param theHandConfidence The confidence in the state of the hand.
 @returns The confidence of the hand state as a string. */
static const char *
handConfidenceToString(const TrackingConfidence theHandConfidence)
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
#endif // ! defined(MpM_BuildDummyServices)

#if (! defined(MpM_BuildDummyServices))
# if defined(GENERATE_BONES_)
/*! @brief Add a bone to the list that's being built.
 @param str_ The name for the bone.
 @param start_ The starting joint for the bone.
 @param end_ The ending joint for the bone. */
#  define ADD_BONE_TO_LIST_(str_, start_, end_) \
        addBoneToList(*bonesList, str_, jointData[start_], jointData[end_],\
                        orientationData[start_], orientationData[end_])

# else // ! defined(GENERATE_BONES_)
/*! @brief Add a joint to the list that's being built.
 @param str_ The name for the joint.
 @param index_ The joint index. */
#  define ADD_JOINT_TO_LIST_(str_, index_) \
        addJointToList(*jointsList, str_, jointData[index_], orientationData[index_])
# endif // ! defined(GENERATE_BONES_)
#endif // ! defined(MpM_BuildDummyServices)

#if (! defined(MpM_BuildDummyServices))
/*! @brief Add the data for a body to a message.
 @param message The message to be updated with the body data.
 @param jointData The set of joints for the body.
 @param orientationData The orientations of the joints.
 @param leftHandState The state of the left hand.
 @param leftHandConfidence The confidence in the value of the state of the left hand.
 @param rightHandState The state of the right hand.
 @param rightHandConfidence The confidence in the value of the state of the right hand. */
static void
addBodyToMessage(yarp::os::Bottle &       message,
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
    yarp::os::Property & bodyProps = message.addDict();

    bodyProps.put("lefthand", handStateToString(leftHandState));
    bodyProps.put("righthand", handStateToString(rightHandState));
    bodyProps.put("lefthandconfidence", handConfidenceToString(leftHandConfidence));
    bodyProps.put("righthandconfidence", handConfidenceToString(rightHandConfidence));
# if defined(GENERATE_BONES_)
    yarp::os::Value    bones;
    yarp::os::Bottle * bonesList = bones.asList();
# else // ! defined(GENERATE_BONES_)
    yarp::os::Value    joints;
    yarp::os::Bottle * jointsList = joints.asList();
# endif // ! defined(GENERATE_BONES_)
    
# if defined(GENERATE_BONES_)
    if (bonesList)
    {
        // Torso
        ADD_BONE_TO_LIST_("head2neck", JointType_Head, JointType_Neck);
        ADD_BONE_TO_LIST_("neck2spineshoulder", JointType_Neck, JointType_SpineShoulder);
        ADD_BONE_TO_LIST_("spineshoulder2spinemid", JointType_SpineShoulder, JointType_SpineMid);
        ADD_BONE_TO_LIST_("spinemid2spinebase", JointType_SpineMid, JointType_SpineBase);
        ADD_BONE_TO_LIST_("spineshoulder2shoulderright", JointType_SpineShoulder,
                          JointType_ShoulderRight);
        ADD_BONE_TO_LIST_("spineshoulder2shoulderleft", JointType_SpineShoulder,
                          JointType_ShoulderLeft);
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
# else // ! defined(GENERATE_BONES_)
    if (jointsList)
    {
        // Torso
        ADD_JOINT_TO_LIST_("head", JointType_Head);
        ADD_JOINT_TO_LIST_("neck", JointType_Neck);
        ADD_JOINT_TO_LIST_("spineshoulder", JointType_SpineShoulder);
        ADD_JOINT_TO_LIST_("spinemid", JointType_SpineMid);
        ADD_JOINT_TO_LIST_("spinebase", JointType_SpineBase);
        ADD_JOINT_TO_LIST_("shoulderright", JointType_ShoulderRight);
        ADD_JOINT_TO_LIST_("shoulderleft", JointType_ShoulderLeft);
        ADD_JOINT_TO_LIST_("hipright", JointType_HipRight);
        ADD_JOINT_TO_LIST_("hipleft", JointType_HipLeft);

        // Right arm
        ADD_JOINT_TO_LIST_("elbowright", JointType_ElbowRight);
        ADD_JOINT_TO_LIST_("wristright", JointType_WristRight);
        ADD_JOINT_TO_LIST_("handright", JointType_HandRight);
        ADD_JOINT_TO_LIST_("handtipright", JointType_HandTipRight);
        ADD_JOINT_TO_LIST_("thumbright", JointType_ThumbRight);
       
        // Left arm
        ADD_JOINT_TO_LIST_("elbowleft", JointType_ElbowLeft);
        ADD_JOINT_TO_LIST_("wristleft", JointType_WristLeft);
        ADD_JOINT_TO_LIST_("handleft", JointType_HandLeft);
        ADD_JOINT_TO_LIST_("handtipleft", JointType_HandTipLeft);
        ADD_JOINT_TO_LIST_("thumbleft", JointType_ThumbLeft);
        
        // Right leg
        ADD_JOINT_TO_LIST_("kneeright", JointType_KneeRight);
        ADD_JOINT_TO_LIST_("ankleright", JointType_AnkleRight);
        ADD_JOINT_TO_LIST_("footright", JointType_FootRight);

        // Left leg
        ADD_JOINT_TO_LIST_("kneeleft", JointType_KneeLeft);
        ADD_JOINT_TO_LIST_("ankleleft", JointType_AnkleLeft);
        ADD_JOINT_TO_LIST_("footleft", JointType_FootLeft);

        // Add them all
        bodyProps.put("joints", joints);
    }
# endif // ! defined(GENERATE_BONES_)
    OD_LOG_EXIT(); //####
} // addBodyToMessage
#endif // ! defined(MpM_BuildDummyServices)

#if (! defined(MpM_BuildDummyServices))
/*! @brief Process the data returned by the Kinect V2 sensor.
 @param message The message to be updated with the sensor data.
 @param nBodyCount The number of 'bodies' in the sensor data.
 @param ppBodies The sensor data.
 @returns @c true if at least one body was added to the message successfully, and @c false
 otherwise. */
static bool
processBody(yarp::os::Bottle & message,
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
                    addBodyToMessage(message, jointData, orientationData, leftHandState,
                                     leftHandConfidence, rightHandState, rightHandConfidence);
                    result = true;
                }
            }
        }
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // processBody
#endif // ! defined(MpM_BuildDummyServices)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

KinectV2EventThread::KinectV2EventThread(Common::GeneralChannel * outChannel) :
    inherited(),
#if (! defined(MpM_BuildDummyServices))
    _kinectSensor(NULL), _bodyFrameReader(NULL), _bodyFrameSource(NULL),
#endif // ! defined(MpM_BuildDummyServices)
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

void
KinectV2EventThread::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = NULL;
    OD_LOG_OBJEXIT(); //####
} // KinectV2EventThread::clearOutputChannel

#if (! defined(MpM_BuildDummyServices))
HRESULT
KinectV2EventThread::initializeDefaultSensor(void)
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
} // KinectV2EventThread::initializeDefaultSensor
#endif // ! defined(MpM_BuildDummyServices)

void
KinectV2EventThread::processEventData(void)
{
    OD_LOG_OBJENTER(); //####
#if (! defined(MpM_BuildDummyServices))
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
# if defined(MpM_StallOnSendProblem)
                                Stall();
# endif // defined(MpM_StallOnSendProblem)
                            }
                        }
                    }
                }
            }
        }
    }
#endif // ! defined(MpM_BuildDummyServices)
    OD_LOG_OBJEXIT(); //####
} // KinectV2EventThread::processEventData

DEFINE_RUN_(KinectV2EventThread)
{
    OD_LOG_OBJENTER(); //####
    for ( ; ! isStopping(); )
    {
#if (! defined(MpM_BuildDummyServices))
        MSG msg;
#endif // ! defined(MpM_BuildDummyServices)

#if (! defined(MpM_BuildDummyServices))
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
#endif // ! defined(MpM_BuildDummyServices)
        ConsumeSomeTime();
    }
    OD_LOG_OBJEXIT(); //####
} // KinectV2EventThread::run

DEFINE_THREADINIT_(KinectV2EventThread)
{
    OD_LOG_OBJENTER(); //####
#if defined(MpM_BuildDummyServices)
    bool result = true;
#else // ! defined(MpM_BuildDummyServices)
    bool result = SUCCEEDED(initializeDefaultSensor());
#endif // ! defined(MpM_BuildDummyServices)

    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // KinectV2EventThread::threadInit

DEFINE_THREADRELEASE_(KinectV2EventThread)
{
    OD_LOG_OBJENTER(); //####
#if (! defined(MpM_BuildDummyServices))
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
#endif // ! defined(MpM_BuildDummyServices)
    OD_LOG_OBJEXIT(); //####
} // KinectV2EventThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
