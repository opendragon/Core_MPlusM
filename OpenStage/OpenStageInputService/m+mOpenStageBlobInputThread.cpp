//--------------------------------------------------------------------------------------------------
//
//  File:       m+mOpenStageBlobInputThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a thread that generates output from Organic Motion
//                OpenStage Blob data.
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
//  Created:    2015-07-14
//
//--------------------------------------------------------------------------------------------------

#include "m+mOpenStageBlobInputThread.h"

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
 @brief The class definition for a thread that generates output from Organic Motion %OpenStage %Blob
 data. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::OpenStageBlob;
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

OpenStageBlobInputThread::OpenStageBlobInputThread(Common::GeneralChannel * outChannel,
                                                   const YarpString &       name,
                                                   const int                port) :
    inherited(), _address(name), _scale(1), _port(port), _outChannel(outChannel)
#if (! defined(MpM_BuildDummyServices))
    , _client(NULL), _actorStream(NULL), _actorViewJoint(NULL)
#endif // ! defined(MpM_BuildDummyServices)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("outChannel = ", outChannel); //####
    OD_LOG_S1s("name = ", name); //####
    OD_LOG_LL1("port = ", port); //####
    OD_LOG_EXIT_P(this); //####
} // OpenStageBlobInputThread::OpenStageBlobInputThread

OpenStageBlobInputThread::~OpenStageBlobInputThread(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // OpenStageBlobInputThread::~OpenStageBlobInputThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
OpenStageBlobInputThread::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = NULL;
    OD_LOG_OBJEXIT(); //####
} // OpenStageBlobInputThread::clearOutputChannel

#if (! defined(MpM_BuildDummyServices))
void
OpenStageBlobInputThread::processData(om::sdk2::ActorDataListConstPtr & actorData)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("actorData = ", &actorData); //####
    size_t numActors = actorData->GetSize();

    if (0 < numActors)
    {
# if (! defined(MpM_UseCustomStringBuffer))
        std::stringstream outBuffer;
# endif // ! defined(MpM_UseCustomStringBuffer)

        // Write out the number of actors == bodies.
# if defined(MpM_UseCustomStringBuffer)
        _outBuffer.reset().addLong(static_cast<int>(numActors)).addString(LINE_END_);
# else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << numActors << LINE_END_;
# endif // ! defined(MpM_UseCustomStringBuffer)
        for (size_t ii = 0; ii < numActors; ++ii)
        {
            sdk2::SkeletonConstPtr skel = actorData->GetAt(ii).skeleton;
            sdk2::JointTreePtr     absJointTree = sdk2::CreateJointTree();
            sdk2::JointTreePtr     relJointTree = sdk2::CreateJointTree();

            sdk2::ConvertSkeletonToJointTreeAbsolute(skel, &absJointTree);
            sdk2::ConvertSkeletonToJointTreeRelative(skel, &relJointTree);
            sdk2::JointTreeConstIterator absJointTreeIt = absJointTree->Begin();
            sdk2::JointTreeConstIterator absJointTreeItEnd = absJointTree->End();
            sdk2::JointTreeConstIterator relJointTreeItEnd = relJointTree->End();
            int count = 0;

            for ( ; absJointTreeIt != absJointTreeItEnd; ++absJointTreeIt)
            {
                const char *                 jointId = *absJointTreeIt->first;
                sdk2::JointTreeConstIterator relJointTreeIt = relJointTree->Find(jointId);

                if (relJointTreeItEnd != relJointTreeIt)
                {
                    ++count;
                }
            }
# if defined(MpM_UseCustomStringBuffer)
            _outBuffer.addLong(static_cast<int>(ii)).addTab().addLong(count).
                addString(LINE_END_);
# else // ! defined(MpM_UseCustomStringBuffer)
            outBuffer << ii << "\t" << count << LINE_END_;
# endif // ! defined(MpM_UseCustomStringBuffer)
            for (absJointTreeIt = absJointTree->Begin(); absJointTreeIt != absJointTreeItEnd;
                 ++absJointTreeIt)
            {
                const char *                 jointId = *absJointTreeIt->first;
                sdk2::JointTreeConstIterator relJointTreeIt = relJointTree->Find(jointId);

                if (relJointTreeItEnd != relJointTreeIt)
                {
                    sdk2::Matrix44 absTransform = absJointTreeIt->second->transform;
                    sdk2::Matrix44 relTransform = relJointTreeIt->second->transform;
                    glm::mat3x3    absJointTransform(absTransform.m[0][0], absTransform.m[0][1],
                                                     absTransform.m[0][2], absTransform.m[1][0],
                                                     absTransform.m[1][1], absTransform.m[1][2],
                                                     absTransform.m[2][0], absTransform.m[2][1],
                                                     absTransform.m[2][2]);
                    glm::mat3x3    relJointTransform(relTransform.m[0][0], relTransform.m[0][1],
                                                     relTransform.m[0][2], relTransform.m[1][0],
                                                     relTransform.m[1][1], relTransform.m[1][2],
                                                     relTransform.m[2][0], relTransform.m[2][1],
                                                     relTransform.m[2][2]);
                    glm::quat      absRotQuat = glm::quat_cast(absJointTransform);
                    glm::quat      relRotQuat = glm::quat_cast(relJointTransform);

# if defined(MpM_UseCustomStringBuffer)
                    _outBuffer.addString(jointId).addTab();
                    _outBuffer.addDouble(absTransform.m[3][0] * _scale).addTab();
                    _outBuffer.addDouble(absTransform.m[3][1] * _scale).addTab();
                    _outBuffer.addDouble(absTransform.m[3][2] * _scale).addTab();
                    _outBuffer.addDouble(absRotQuat.x).addTab();
                    _outBuffer.addDouble(absRotQuat.y).addTab();
                    _outBuffer.addDouble(absRotQuat.z).addTab();
                    _outBuffer.addDouble(absRotQuat.w).addTab();
                    _outBuffer.addDouble(relTransform.m[3][0] * _scale).addTab();
                    _outBuffer.addDouble(relTransform.m[3][1] * _scale).addTab();
                    _outBuffer.addDouble(relTransform.m[3][2] * _scale).addTab();
                    _outBuffer.addDouble(relRotQuat.x).addTab();
                    _outBuffer.addDouble(relRotQuat.y).addTab();
                    _outBuffer.addDouble(relRotQuat.z).addTab();
                    _outBuffer.addDouble(relRotQuat.w).addString(LINE_END_);
# else // ! defined(MpM_UseCustomStringBuffer)
                    outBuffer << jointId << "\t" << (absTransform.m[3][0] * _scale) << "\t" <<
                        (absTransform.m[3][1] * _scale) << "\t" <<
                        (absTransform.m[3][2] * _scale) << "\t" << absRotQuat.x << "\t" <<
                        absRotQuat.y << "\t" << absRotQuat.z << "\t" << absRotQuat.w <<
                        "\t" << (relTransform.m[3][0] * _scale) << "\t" <<
                        (relTransform.m[3][1] * _scale) << "\t" <<
                        (relTransform.m[3][2] * _scale) << "\t" << relRotQuat.x << "\t" <<
                        relRotQuat.y << "\t" << relRotQuat.z << "\t" << relRotQuat.w <<
                        LINE_END_;
# endif // ! defined(MpM_UseCustomStringBuffer)
                }
            }
        }
# if defined(MpM_UseCustomStringBuffer)
        _outBuffer.addString("END" LINE_END_);
# else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << "END" LINE_END_;
# endif // ! defined(MpM_UseCustomStringBuffer)
        if (_outChannel)
        {
            const char *     outString;
            yarp::os::Bottle message;
            size_t           outLength;
# if (! defined(MpM_UseCustomStringBuffer))
            std::string      buffAsString(outBuffer.str());
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
                    OD_LOG("(! _outChannel->write(_messageBottle))"); //####
# if defined(MpM_StallOnSendProblem)
                    Stall();
# endif // defined(MpM_StallOnSendProblem)
                }
            }
        }
    }
    OD_LOG_OBJEXIT(); //####
} // OpenStageBlobInputThread::processData
#endif // ! defined(MpM_BuildDummyServices)

DEFINE_RUN_(OpenStageBlobInputThread)
{
    OD_LOG_OBJENTER(); //####
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
        yarp::os::Time::yield();
    }
    OD_LOG_OBJEXIT(); //####
} // OpenStageBlobInputThread::run

void
OpenStageBlobInputThread::setScale(const double newScale)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_D1("newScale = ", newScale); //####
    _scale = newScale;
    OD_LOG_OBJEXIT(); //####
} // OpenStageBlobInputThread::setScale

DEFINE_THREADINIT_(OpenStageBlobInputThread)
{
    OD_LOG_OBJENTER(); //####
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
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // OpenStageBlobInputThread::threadInit

DEFINE_THREADRELEASE_(OpenStageBlobInputThread)
{
    OD_LOG_OBJENTER(); //####
#if (! defined(MpM_BuildDummyServices))
    _actorStream->Stop();
#endif // ! defined(MpM_BuildDummyServices)
    OD_LOG_OBJEXIT(); //####
} // OpenStageBlobInputThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
