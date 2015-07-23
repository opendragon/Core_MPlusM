//--------------------------------------------------------------------------------------------------
//
//  File:       m+mOpenStageBlobInputThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a thread that generates output from Organic Motion
//				OpenStage Blob data.
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
#include <glm/glm.hpp>
#include <glm/gtc/quaternion.hpp>
#include <glm/gtc/matrix_transform.hpp>
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
using namespace om;

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
	inherited(), _address(name), _port(port), _outChannel(outChannel), _client(NULL),
	_actorStream(NULL), _actorViewJoint(NULL)
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

void OpenStageBlobInputThread::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = NULL;
    OD_LOG_OBJEXIT(); //####
} // OpenStageBlobInputThread::clearOutputChannel

void OpenStageBlobInputThread::processData(om::sdk2::ActorDataListConstPtr & actorData)
{
	OD_LOG_OBJENTER(); //####
	OD_LOG_P1("actorData = ", &actorData); //####
	size_t numActors = actorData->GetSize();

	if (0 < numActors)
	{
		std::stringstream outBuffer;

		// Write out the number of actors == bodies.
		outBuffer << numActors << LINE_END_;
		for (size_t ii = 0; ii < numActors; ++ii)
		{
			sdk2::SkeletonConstPtr skel = actorData->GetAt(ii).skeleton;
			sdk2::JointTreePtr     jointTree = sdk2::CreateJointTree();

			sdk2::ConvertSkeletonToJointTreeAbsolute(skel, &jointTree);
			sdk2::JointTreeConstIterator jointTreeIt = jointTree->Begin();
			sdk2::JointTreeConstIterator jointTreeItEnd = jointTree->End();

			outBuffer << ii << "\t" << jointTree->GetSize() << LINE_END_;
			for ( ; jointTreeIt != jointTreeItEnd; ++jointTreeIt)
			{
				sdk2::Matrix44 transform = jointTreeIt->second->transform;
				glm::mat3x3    jointTransform(transform.m[0][0], transform.m[0][1],
											  transform.m[0][2], transform.m[1][0],
											  transform.m[1][1], transform.m[1][2],
					                          transform.m[2][0], transform.m[2][1],
											  transform.m[2][2]);
				glm::quat      rotQuat = glm::quat_cast(jointTransform);

				outBuffer << *jointTreeIt->first << "\t" << (transform.m[3][0] * _scale) << "\t" <<
							(transform.m[3][1] * _scale) << "\t" << (transform.m[3][2] * _scale) <<
                            "\t" << rotQuat.x << "\t" << rotQuat.y << "\t" << rotQuat.z << "\t" <<
                            rotQuat.w << LINE_END_;

			}
		}
		outBuffer << "END" << LINE_END_;
		if (_outChannel)
		{
			yarp::os::Bottle message;
			std::string      buffAsString(outBuffer.str());
			yarp::os::Value  blobValue(const_cast<char *>(buffAsString.c_str()),
									   static_cast<int>(buffAsString.length()));

			message.add(blobValue);
			if (!_outChannel->write(message))
			{
				OD_LOG("(! _outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
				Stall();
#endif // defined(MpM_StallOnSendProblem)
			}
		}
	}
	OD_LOG_OBJEXIT(); //####
} // OpenStageBlobInputThread::processData

void OpenStageBlobInputThread::run(void)
{
    OD_LOG_OBJENTER(); //####
	_actorStream->Start();
	for ( ; ! isStopping(); )
    {
		if (_client->WaitAnyUpdateAll())
		{
			// The streams are updated, now get the data.
			sdk2::ActorDataListConstPtr actorData;

			_actorStream->GetData(&actorData);
			processData(actorData);
		}
        yarp::os::Time::yield();
    }
    OD_LOG_OBJEXIT(); //####
} // OpenStageBlobInputThread::run

void OpenStageBlobInputThread::setScale(const double newScale)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_D1("newScale = ", newScale); //####
    _scale = newScale;
    OD_LOG_OBJEXIT(); //####
} // OpenStageBlobInputThread::setScale

bool OpenStageBlobInputThread::threadInit(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = true;
    
	// Create the necessary objects.
	_client = sdk2::CreateClient();

	// Connect to the device.
	_client->SetEndpoint(_address.c_str(), _port);
	_actorStream = sdk2::CreateActorStream(_client);
	_actorViewJoint = sdk2::CreateActorViewJoint();
	_actorStream->SetBufferSize(ACTOR_QUEUE_DEPTH_);
	OD_LOG_OBJEXIT_B(result); //####
    return result;
} // OpenStageBlobInputThread::threadInit

void OpenStageBlobInputThread::threadRelease(void)
{
    OD_LOG_OBJENTER(); //####
	_actorStream->Stop();
    OD_LOG_OBJEXIT(); //####
} // OpenStageBlobInputThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
