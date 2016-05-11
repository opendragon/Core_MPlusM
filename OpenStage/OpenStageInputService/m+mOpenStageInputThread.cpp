//--------------------------------------------------------------------------------------------------
//
//  File:       m+mOpenStageInputThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a thread that generates output from Organic Motion
//                OpenStage data.
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
//  Created:    2015-06-25
//
//--------------------------------------------------------------------------------------------------

#include "m+mOpenStageInputThread.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wshadow"
#endif // defined(__APPLE__)
#if (! defined(MpM_BuildDummyServices))
# include <glm/glm.hpp>
# include <glm/gtc/quaternion.hpp>
# include <glm/gtc/matrix_transform.hpp>
#endif // ! defined(MpM_BuildDummyServices)
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a thread that generates output from Organic Motion %OpenStage
 data. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::OpenStage;
#if (! defined(MpM_BuildDummyServices))
using namespace om;
#endif // ! defined(MpM_BuildDummyServices)

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The number if actor stream reports to buffer. */
#define ACTOR_QUEUE_DEPTH_ 2

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

OpenStageInputThread::OpenStageInputThread(Common::GeneralChannel * outChannel,
                                           const YarpString &       name,
                                           const int                port) :
    inherited(), _address(name), _port(port), _outChannel(outChannel)
#if (! defined(MpM_BuildDummyServices))
    , _client(NULL), _actorStream(NULL), _actorViewJoint(NULL)
#endif // ! defined(MpM_BuildDummyServices)
{
    ODL_ENTER(); //####
    ODL_P1("outChannel = ", outChannel); //####
    ODL_S1s("name = ", name); //####
    ODL_LL1("port = ", port); //####
    ODL_EXIT_P(this); //####
} // OpenStageInputThread::OpenStageInputThread

OpenStageInputThread::~OpenStageInputThread(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // OpenStageInputThread::~OpenStageInputThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
OpenStageInputThread::clearOutputChannel(void)
{
    ODL_OBJENTER(); //####
    _outChannel = NULL;
    ODL_OBJEXIT(); //####
} // OpenStageInputThread::clearOutputChannel

#if (! defined(MpM_BuildDummyServices))
void
OpenStageInputThread::processData(om::sdk2::ActorDataListConstPtr & actorData)
{
    ODL_OBJENTER(); //####
    ODL_P1("actorData = ", &actorData); //####
    size_t numActors = actorData->GetSize();

    if (0 < numActors)
    {
        yarp::os::Bottle   message;
        yarp::os::Bottle & actorList = message.addList();

        for (size_t ii = 0; ii < numActors; ++ii)
        {
            yarp::os::Bottle &     anActor = actorList.addList();
            sdk2::SkeletonConstPtr skel = actorData->GetAt(ii).skeleton;
            sdk2::JointTreePtr     absJointTree = sdk2::CreateJointTree();
            sdk2::JointTreePtr     relJointTree = sdk2::CreateJointTree();

            sdk2::ConvertSkeletonToJointTreeAbsolute(skel, &absJointTree);
            sdk2::ConvertSkeletonToJointTreeRelative(skel, &relJointTree);
            sdk2::JointTreeConstIterator jointTreeAbsIt = absJointTree->Begin();
            sdk2::JointTreeConstIterator jointTreeAbsItEnd = absJointTree->End();
            sdk2::JointTreeConstIterator jointTreeRelItEnd = relJointTree->End();

            for ( ; jointTreeAbsIt != jointTreeAbsItEnd; ++jointTreeAbsIt)
            {
                yarp::os::Property & jointProps = anActor.addDict();
                sdk2::Matrix44       absTransform = jointTreeAbsIt->second->transform;
                const char *         jointId = static_cast<const char *>(*jointTreeAbsIt->first);
                yarp::os::Value      absPosStuff;
                yarp::os::Bottle *   absPosList = absPosStuff.asList();

                jointProps.put("id", jointId);
                if (absPosList)
                {
                    absPosList->addDouble(absTransform.m[3][0]);
                    absPosList->addDouble(absTransform.m[3][1]);
                    absPosList->addDouble(absTransform.m[3][2]);
                    jointProps.put("absposition", absPosStuff);
                }
                yarp::os::Value    absQuatStuff;
                yarp::os::Bottle * absQuatList = absQuatStuff.asList();

                if (absQuatList)
                {
                    glm::mat3x3 jointAbsTransform(absTransform.m[0][0], absTransform.m[0][1],
                                                  absTransform.m[0][2], absTransform.m[1][0],
                                                  absTransform.m[1][1], absTransform.m[1][2],
                                                  absTransform.m[2][0], absTransform.m[2][1],
                                                  absTransform.m[2][2]);
                    glm::quat   absRotQuat = glm::quat_cast(jointAbsTransform);

                    absQuatList->addDouble(absRotQuat.x);
                    absQuatList->addDouble(absRotQuat.y);
                    absQuatList->addDouble(absRotQuat.z);
                    absQuatList->addDouble(absRotQuat.w);
                    jointProps.put("absrotation", absQuatStuff);
                }
                sdk2::JointTreeConstIterator jointTreeRelIt = relJointTree->Find(jointId);

                if (jointTreeRelIt != jointTreeRelItEnd)
                {
                    sdk2::Matrix44     relTransform = jointTreeRelIt->second->transform;
                    yarp::os::Value    relPosStuff;
                    yarp::os::Bottle * relPosList = relPosStuff.asList();

                    if (relPosList)
                    {
                        relPosList->addDouble(relTransform.m[3][0]);
                        relPosList->addDouble(relTransform.m[3][1]);
                        relPosList->addDouble(relTransform.m[3][2]);
                        jointProps.put("relposition", relPosStuff);
                    }
                    yarp::os::Value    relQuatStuff;
                    yarp::os::Bottle * relQuatList = relQuatStuff.asList();

                    if (relQuatList)
                    {
                        glm::mat3x3 jointRelTransform(relTransform.m[0][0], relTransform.m[0][1],
                                                      relTransform.m[0][2], relTransform.m[1][0],
                                                      relTransform.m[1][1], relTransform.m[1][2],
                                                      relTransform.m[2][0], relTransform.m[2][1],
                                                      relTransform.m[2][2]);
                        glm::quat   relRotQuat = glm::quat_cast(jointRelTransform);

                        relQuatList->addDouble(relRotQuat.x);
                        relQuatList->addDouble(relRotQuat.y);
                        relQuatList->addDouble(relRotQuat.z);
                        relQuatList->addDouble(relRotQuat.w);
                        jointProps.put("relrotation", relQuatStuff);
                    }
                }
            }
        }
        if (_outChannel)
        {
            if (! _outChannel->write(message))
            {
                ODL_LOG("(! _outChannel->write(message))"); //####
# if defined(MpM_StallOnSendProblem)
                Stall();
# endif // defined(MpM_StallOnSendProblem)
            }
        }
    }
    ODL_OBJEXIT(); //####
} // OpenStageInputThread::processData
#endif // ! defined(MpM_BuildDummyServices)

void
OpenStageInputThread::run(void)
{
    ODL_OBJENTER(); //####
#if (! defined(MpM_BuildDummyServices))
    _actorStream->Start();
#endif // ! defined(MpM_BuildDummyServices)
    for ( ; ! isStopping(); )
    {
#if (! defined(MpM_BuildDummyServices))
        if (_client->WaitAnyUpdateAll())
        {
            // The streams are updated, now get the data.
            sdk2::ActorDataListConstPtr actorData;

            _actorStream->GetData(&actorData);
            processData(actorData);
        }
#endif // ! defined(MpM_BuildDummyServices)
        ConsumeSomeTime();
    }
    ODL_OBJEXIT(); //####
} // OpenStageInputThread::run

bool
OpenStageInputThread::threadInit(void)
{
    ODL_OBJENTER(); //####
    bool result = true;

#if (! defined(MpM_BuildDummyServices))
    // Create the necessary objects.
    _client = sdk2::CreateClient();

    // Connect to the device.
    _client->SetEndpoint(_address.c_str(), _port);
    _actorStream = sdk2::CreateActorStream(_client);
    _actorViewJoint = sdk2::CreateActorViewJoint();
    _actorStream->SetBufferSize(ACTOR_QUEUE_DEPTH_);
#endif // ! defined(MpM_BuildDummyServices)
    ODL_OBJEXIT_B(result); //####
    return result;
} // OpenStageInputThread::threadInit

void
OpenStageInputThread::threadRelease(void)
{
    ODL_OBJENTER(); //####
#if (! defined(MpM_BuildDummyServices))
    _actorStream->Stop();
#endif // ! defined(MpM_BuildDummyServices)
    ODL_OBJEXIT(); //####
} // OpenStageInputThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
