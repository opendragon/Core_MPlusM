//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRecordBlobOutputInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the input channel input handler used by the Record Blob
//              output service.
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

#include "m+mRecordBlobOutputInputHandler.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the input channel input handler used by the Record %Blob output
 service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::RecordBlob;
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

RecordBlobOutputInputHandler::RecordBlobOutputInputHandler(void) :
    inherited(), _outFile(NULL)
{
    ODL_ENTER(); //####
    ODL_EXIT_P(this); //####
} // RecordBlobOutputInputHandler::RecordBlobOutputInputHandler

RecordBlobOutputInputHandler::~RecordBlobOutputInputHandler(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // RecordBlobOutputInputHandler::~RecordBlobOutputInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
DEFINE_HANDLE_INPUT_(RecordBlobOutputInputHandler)
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
        if (_outFile)
        {
            ODL_LOG("(_outFile)"); //####
            if (1 == input.size())
            {
                yarp::os::Value & firstTopValue = input.get(0);

                if (firstTopValue.isBlob())
                {
                    size_t       numBytes = firstTopValue.asBlobLength();
                    const char * asBytes = firstTopValue.asBlob();

                    if ((0 < numBytes) && asBytes)
                    {
                        size_t retVal = fwrite(asBytes, numBytes, 1, _outFile);

                        if (retVal == numBytes)
                        {
                            fflush(_outFile);
                        }
                        else
                        {
                            cerr << "Write error" << endl; //!!!!
                        }
                    }
                    else
                    {
                        cerr << "Bad blob" << endl; //!!!!
                    }
                }
                else
                {
                    cerr << "Input not just a blob" << endl; //!!!!
                }
            }
            else
            {
                cerr << "Input not just a single blob" << endl; //!!!!
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
} // RecordBlobOutputInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
RecordBlobOutputInputHandler::setFile(FILE * outFile)
{
    ODL_OBJENTER(); //####
    ODL_P1("outFile = ", outFile); //####
    _outFile = outFile;
    ODL_OBJEXIT(); //####
} // RecordBlobOutputInputHandler::setFile

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
