//--------------------------------------------------------------------------------------------------
//
//  File:       m+mBlobOutputInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the input handler used by the Blob output service.
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
//  Created:    2015-06-23
//
//--------------------------------------------------------------------------------------------------

#include "m+mBlobOutputInputHandler.hpp"
#include "m+mBlobOutputService.hpp"

//#include <ODEnableLogging.h>
#include <ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the input handler used by the %Blob output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Blob;
using namespace MplusM::Common;
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

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

BlobOutputInputHandler::BlobOutputInputHandler(BlobOutputService & owner) :
    inherited(), _owner(owner), _outSocket(INVALID_SOCKET)
{
    ODL_ENTER(); //####
    ODL_P1("owner = ", &owner); //####
    ODL_EXIT_P(this); //####
} // BlobOutputInputHandler::BlobOutputInputHandler

BlobOutputInputHandler::~BlobOutputInputHandler(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // BlobOutputInputHandler::~BlobOutputInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool
BlobOutputInputHandler::handleInput(const yarp::os::Bottle &     input,
                                    const YarpString &           senderChannel,
                                    yarp::os::ConnectionWriter * replyMechanism,
                                    const size_t                 numBytes)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(senderChannel,replyMechanism)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_S2s("senderChannel = ", senderChannel, "got ", input.toString()); //####
    ODL_P1("replyMechanism = ", replyMechanism); //####
    ODL_L1("numBytes = ", numBytes); //####
    bool result = true;

    try
    {
        if (_owner.isActive())
        {
            if (INVALID_SOCKET == _outSocket)
            {
                cerr << "Invalid socket." << endl;
            }
            else
            {
                if (1 == input.size())
                {
                    yarp::os::Value & firstTopValue = input.get(0);

                    if (firstTopValue.isBlob())
                    {
                        size_t       numBytes = firstTopValue.asBlobLength();
                        const char * asBytes = firstTopValue.asBlob();

                        if ((0 < numBytes) && asBytes)
                        {
                            int retVal = send(_outSocket, asBytes, static_cast<int>(numBytes), 0);

                            cerr << "send--> " << retVal << endl; //!!!!
                            if (0 > retVal)
                            {
                                _owner.deactivateConnection();
                            }
                            else
                            {
                                SendReceiveCounters toBeAdded(0, 0, numBytes, 1);

                                _owner.incrementAuxiliaryCounters(toBeAdded);
                            }
                        }
                        else
                        {
                            cerr << "Bad blob." << endl;
                        }
                    }
                    else
                    {
                        cerr << "Input not just a blob." << endl;
                    }
                }
                else
                {
                    cerr << "Input not just a single blob." << endl;
                }
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // BlobOutputInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
BlobOutputInputHandler::setSocket(const SOCKET outSocket)
{
    ODL_OBJENTER(); //####
    ODL_L1("outSocket = ", outSocket); //####
    _outSocket = outSocket;
    ODL_OBJEXIT(); //####
} // BlobOutputInputHandler::setSocket

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
