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

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if 0
/*! @brief Add a floating-point vector to a dictionary.
@param dictionary The dictionary to be updated.
@param tag The tag to associate with the vector.
@param position The vector to be added. */
static void addVectorToDictionary(yarp::os::Property &          dictionary,
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
} // addVectorToDictionary

/*! @brief Add the description of a bone to a list.
@param listToUpdate The list to be added to.
@param joinTag The name of the bone.
@param startJoint The beginning joint of the bone.
@param endJoint The ending joint of the bone. */
static void addBoneToList(yarp::os::Bottle &            listToUpdate,
                          const yarp::os::ConstString & jointTag,
                          const Joint &                 startJoint,
                          const Joint &                 endJoint)
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
            addVectorToDictionary(boneProps, "start", startJoint.Position);
            addVectorToDictionary(boneProps, "end", endJoint.Position);
        }
    }
} // addBoneToList

/*! @brief Convert a hand state into a string.
@param theHandState The state of the hand
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

/* !brief Add the data for a body to a message.
@param message The message to be updated with the body data
@param joints The set of joints for the body
@param leftHandState The state of the left hand
@param rightHandState The state of the right hand */
static void addBodyToMessage(yarp::os::Bottle & message,
                             const Joint *      joints,
                             const HandState    leftHandState,
                             const HandState    rightHandState)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("message = ", &message, "joints = ", joints); //####
    yarp::os::Property & bodyProps = message.addDict();

    bodyProps.put("lefthand", handStateToString(leftHandState));
    bodyProps.put("righthand", handStateToString(rightHandState));
    yarp::os::Value    bones;
    yarp::os::Bottle * bonesList = bones.asList();

    if (bonesList)
    {
#if 0
        // Torso
        addBoneToList(*bonesList, "head2neck", joints[JointType_Head], joints[JointType_Neck]);
        addBoneToList(*bonesList, "neck2spineshoulder", joints[JointType_Neck], joints[JointType_SpineShoulder]);
        addBoneToList(*bonesList, "spineshoulder2spinemid", joints[JointType_SpineShoulder], joints[JointType_SpineMid]);
        addBoneToList(*bonesList, "spinemid2spinebase", joints[JointType_SpineMid], joints[JointType_SpineBase]);
        addBoneToList(*bonesList, "spineshoulder2shoulderright", joints[JointType_SpineShoulder], joints[JointType_ShoulderRight]);
        addBoneToList(*bonesList, "spineshoulder2shoulderleft", joints[JointType_SpineShoulder], joints[JointType_ShoulderLeft]);
        addBoneToList(*bonesList, "spinebase2hipright", joints[JointType_SpineBase], joints[JointType_HipRight]);
        addBoneToList(*bonesList, "spinebase2hipleft", joints[JointType_SpineBase], joints[JointType_HipLeft]);
#endif//0
        // Right arm
        addBoneToList(*bonesList, "shoulderright2elbowright", joints[JointType_ShoulderRight], joints[JointType_ElbowRight]);
        addBoneToList(*bonesList, "elbowright2wristright", joints[JointType_ElbowRight], joints[JointType_WristRight]);
        addBoneToList(*bonesList, "wristright2handright", joints[JointType_WristRight], joints[JointType_HandRight]);
        addBoneToList(*bonesList, "handright2handtipright", joints[JointType_HandRight], joints[JointType_HandTipRight]);
        addBoneToList(*bonesList, "wristright2thumbright", joints[JointType_WristRight], joints[JointType_ThumbRight]);
#if 0
        // Left arm
        addBoneToList(*bonesList, "shoulderleft2elbowleft", joints[JointType_ShoulderLeft], joints[JointType_ElbowLeft]);
        addBoneToList(*bonesList, "elbowleft2wristleft", joints[JointType_ElbowLeft], joints[JointType_WristLeft]);
        addBoneToList(*bonesList, "wristleft2handleft", joints[JointType_WristLeft], joints[JointType_HandLeft]);
        addBoneToList(*bonesList, "handleft2handtipleft", joints[JointType_HandLeft], joints[JointType_HandTipLeft]);
        addBoneToList(*bonesList, "wristleft2thumbleft", joints[JointType_WristLeft], joints[JointType_ThumbLeft]);
        // Right leg
        addBoneToList(*bonesList, "hipright2kneeright", joints[JointType_HipRight], joints[JointType_KneeRight]);
        addBoneToList(*bonesList, "kneeright2ankleright", joints[JointType_KneeRight], joints[JointType_AnkleRight]);
        addBoneToList(*bonesList, "ankleright2footright", joints[JointType_AnkleRight], joints[JointType_FootRight]);
        // Left leg
        addBoneToList(*bonesList, "hipleft2kneeleft", joints[JointType_HipLeft], joints[JointType_KneeLeft]);
        addBoneToList(*bonesList, "kneeleft2ankleleft", joints[JointType_KneeLeft], joints[JointType_AnkleLeft]);
        addBoneToList(*bonesList, "ankleleft2footleft", joints[JointType_AnkleLeft], joints[JointType_FootLeft]);
#endif//0
        // Add them all
        bodyProps.put("bones", bones);
    }
    OD_LOG_EXIT(); //####
} // addBodyToMessage

/*! @brief Process the data returned by the Kinect V2 sensor.
@param message The message to be updated with the sensor data
@param nBodyCount The number of 'bodies' in the sensor data
@param ppBodies The sensor data
@returns @c true if at least one body was added to the message successfully, and @c false otherwise. */
static bool processBody(yarp::os::Bottle & message,
                        const int          nBodyCount,
                        IBody **           ppBodies)
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
                Joint     joints[JointType_Count]; 
                HandState leftHandState = HandState_Unknown;
                HandState rightHandState = HandState_Unknown;

                pBody->get_HandLeftState(&leftHandState);
                pBody->get_HandRightState(&rightHandState);
                hr = pBody->GetJoints(_countof(joints), joints);
                if (SUCCEEDED(hr))
                {
                    addBodyToMessage(message, joints, leftHandState, rightHandState);
                    result = true;
                }
            }
        }
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // processBody
#endif//0

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

KinectV2EventThread::KinectV2EventThread(/*Common::GeneralChannel * outChannel*/) :
    inherited(), _kinectSensor(NULL), _bodyFrameReader(NULL), _bodyFrameSource(NULL)/*, _outChannel(outChannel)*/
{
    OD_LOG_ENTER(); //####
//    OD_LOG_P1("outChannel = ", outChannel); //####
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

#if 0
void KinectV2InputThread::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = nullptr;
    OD_LOG_OBJEXIT(); //####
} // KinectV2InputThread::clearOutputChannel
#endif //0

HRESULT KinectV2EventThread::initializeDefaultSensor(void)
{
    OD_LOG_OBJENTER(); //####
    HRESULT hr = GetDefaultKinectSensor(&_kinectSensor);

    if (! FAILED(hr))
    {
        if (_kinectSensor)
        {
            // Initialize the Kinect and get coordinate mapper and the body reader
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
                hr = _bodyFrameSource->SubscribeFrameCaptured(&_frameEventHandle);
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
                    std::cerr << "got one!" << std::endl;
                    break;

                default :
                    break;
            }
        }
        if (WM_QUIT == msg.message)
        {
            stop();
        }
#if 0
        yarp::os::Bottle message;

        if (updateData(message) && _outChannel)
        {
            if (! _outChannel->write(message))
            {
                OD_LOG("(! _outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                Stall();
#endif // defined(MpM_StallOnSendProblem)
            }
        }
#endif//0
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
    if (_bodyFrameSource && _frameEventHandle)
    {
        _bodyFrameSource->UnsubscribeFrameCaptured(_frameEventHandle);
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

#if 0
bool KinectV2InputThread::updateData(yarp::os::Bottle & message)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("message = ", message); //####
    bool result = false;

    if (_bodyFrameReader)
    {
        IBodyFrame * pBodyFrame = nullptr;
        HRESULT      hr = _bodyFrameReader->AcquireLatestFrame(&pBodyFrame);

        if (SUCCEEDED(hr))
        {
            IBody * ppBodies[BODY_COUNT] = { nullptr };

            if (SUCCEEDED(hr))
            {
                hr = pBodyFrame->GetAndRefreshBodyData(_countof(ppBodies), ppBodies);
            }
            if (SUCCEEDED(hr))
            {
                result = processBody(message, BODY_COUNT, ppBodies);
            }
            for (int ii = 0; _countof(ppBodies) > ii; ++ii)
            {
                SafeRelease(ppBodies[ii]);
            }
        }
        SafeRelease(pBodyFrame);
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // KinectV2InputThread::updateData
#endif//0

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
