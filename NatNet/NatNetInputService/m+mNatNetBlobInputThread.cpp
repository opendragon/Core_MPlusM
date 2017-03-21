//--------------------------------------------------------------------------------------------------
//
//  File:       m+mNatNetBlobInputThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for an output-generating thread for m+m.
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
//  Created:    2015-07-23
//
//--------------------------------------------------------------------------------------------------

#include "m+mNatNetBlobInputThread.hpp"

//#include <ODEnableLogging.h>
#include <ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for an output-generating thread for m+m. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::NatNet;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

//#define REPORT_NATNET_MESSAGES_ /* if defined, be verbose */

/*! @brief The connection mode for the client object. */
#define NATNET_CONNECTION_MODE_ 0 /* 0=multicast, 1=unicast */

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if (! defined(MpM_BuildDummyServices))
/*! @brief Process a received frame of data.
@param[in] aFrame The data to be processed.
@param[in] userData The initially supplied data for this callback. */
static void __cdecl
dataReceived(sFrameOfMocapData * aFrame,
             void *              userData)
{
    ODL_ENTER(); //####
    ODL_P2("aFrame = ", aFrame, "userData = ", userData); //####
    NatNetBlobInputThread * theThread = reinterpret_cast<NatNetBlobInputThread *>(userData);

    if (aFrame && theThread)
    {
        double scale = theThread->translationScale();
        int    numSkeletons = aFrame->nSkeletons;

        if (0 < numSkeletons)
        {
# if defined(MpM_UseCustomStringBuffer)
            Common::StringBuffer & outBuffer = theThread->getOutputBuffer();
# else // ! defined(MpM_UseCustomStringBuffer)
            std::stringstream      outBuffer;
# endif // ! defined(MpM_UseCustomStringBuffer)

            // Write out the number of skeletons == bodies.
# if defined(MpM_UseCustomStringBuffer)
            outBuffer.reset().addLong(static_cast<int>(numSkeletons)).addString(LINE_END_);
# else // ! defined(MpM_UseCustomStringBuffer)
            outBuffer << numSkeletons << LINE_END_;
# endif // ! defined(MpM_UseCustomStringBuffer)
            for (int ii = 0; numSkeletons > ii; ++ii)
            {
                sSkeletonData &  aSkel = aFrame->Skeletons[ii];
                int              numBones = aSkel.nRigidBodies;

# if defined(MpM_UseCustomStringBuffer)
                outBuffer.addLong(ii).addTab().addLong(numBones).addString(LINE_END_);
# else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << ii << "\t" << numBones << LINE_END_;
# endif // ! defined(MpM_UseCustomStringBuffer)
                for (int jj = 0; numBones > jj; ++jj)
                {
                    sRigidBodyData & aBone = aSkel.RigidBodyData[jj];

# if defined(MpM_UseCustomStringBuffer)
                    outBuffer.addLong(aBone.ID).addTab();
                    outBuffer.addDouble(aBone.x * scale).addTab();
                    outBuffer.addDouble(aBone.y * scale).addTab();
                    outBuffer.addDouble(aBone.z * scale).addTab();
                    outBuffer.addDouble(aBone.qx).addTab();
                    outBuffer.addDouble(aBone.qy).addTab();
                    outBuffer.addDouble(aBone.qz).addTab();
                    outBuffer.addDouble(aBone.qw).addString(LINE_END_);
# else // ! defined(MpM_UseCustomStringBuffer)
                    outBuffer << aBone.ID << "\t" << (aBone.x * scale) << "\t" <<
                    (aBone.y * scale) << "\t" << (aBone.z * scale) << "\t" <<
                    aBone.qx << "\t" << aBone.qy << "\t" << aBone.qz << "\t" <<
                    aBone.qw << LINE_END_;
# endif // ! defined(MpM_UseCustomStringBuffer)
                }
            }
# if defined(MpM_UseCustomStringBuffer)
            outBuffer.addString("END" LINE_END_);
# else // ! defined(MpM_UseCustomStringBuffer)
            outBuffer << "END" LINE_END_;
# endif // ! defined(MpM_UseCustomStringBuffer)
            const char * outString;
            size_t       outLength;
# if (! defined(MpM_UseCustomStringBuffer))
            std::string  buffAsString(outBuffer.str());
# endif // ! defined(MpM_UseCustomStringBuffer)

# if defined(MpM_UseCustomStringBuffer)
            outString = outBuffer.getString(outLength);
# else // ! defined(MpM_UseCustomStringBuffer)
            outString = bufAsString.c_str();
            outLength = bufAsString.length();
# endif // ! defined(MpM_UseCustomStringBuffer)
            if (outString && outLength)
            {
                theThread->sendMessage(outString, outLength);
            }
        }
    }
    ODL_EXIT(); //####
} // dataReceived
#endif // ! defined(MpM_BuildDummyServices)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
static void __cdecl
messageReceived(int    messageType,
                char * message)
{
#if ((! defined(OD_ENABLE_LOGGING_)) || (! defined(REPORT_NATNET_MESSAGES_)))
# if MAC_OR_LINUX_
#  pragma unused(messageType,message)
# endif // MAC_OR_LINUX_
#endif // (! defined(OD_ENABLE_LOGGING_)) || (! defined(REPORT_NATNET_MESSAGES_))
    ODL_ENTER(); //####
    ODL_LL1("messageType = ", messageType); //####
    ODL_S1("message = ", message); //####
#if defined(REPORT_NATNET_MESSAGES_)
    std::cerr << messageType << ": " << message << std::endl;
#endif // defined(REPORT_NATNET_MESSAGES_)
    ODL_EXIT(); //####
} // messageReceived
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

NatNetBlobInputThread::NatNetBlobInputThread(Common::GeneralChannel * outChannel,
                                             const YarpString &       name,
                                             const int                commandPort,
                                             const int                dataPort) :
    inherited(), _outChannel(outChannel), _address(name), _translationScale(1),
    _commandPort(commandPort), _dataPort(dataPort)
#if (! defined(MpM_BuildDummyServices))
    , _client(NULL)
#endif // ! defined(MpM_BuildDummyServices)
{
    ODL_ENTER(); //####
    ODL_P1("outChannel = ", outChannel); //####
    ODL_S1s("name = ", name); //####
    ODL_LL2("commandPort = ", commandPort, "dataPort = ", dataPort); //####
#if defined(MpM_BuildDummyServices)
    strncpy(_clientIPAddress, "", sizeof(_clientIPAddress) - 1);
    strncpy(_serverIPAddress, "", sizeof(_serverIPAddress) - 1);
#else // ! defined(MpM_BuildDummyServices)
    strcpy_s(_clientIPAddress, sizeof(_clientIPAddress) - 1, "");
    strcpy_s(_serverIPAddress, sizeof(_serverIPAddress) - 1, "");
#endif // ! defined(MpM_BuildDummyServices)
    ODL_EXIT_P(this); //####
} // NatNetBlobInputThread::NatNetBlobInputThread

NatNetBlobInputThread::~NatNetBlobInputThread(void)
{
    ODL_OBJENTER(); //####
#if (! defined(MpM_BuildDummyServices))
    if (_client)
    {
        _client->Uninitialize();
        delete _client;
    }
#endif // ! defined(MpM_BuildDummyServices)
    ODL_OBJEXIT(); //####
} // NatNetBlobInputThread::~NatNetBlobInputThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
NatNetBlobInputThread::clearOutputChannel(void)
{
    ODL_OBJENTER(); //####
    _outChannel = NULL;
    ODL_OBJEXIT(); //####
} // NatNetBlobInputThread::clearOutputChannel

void
NatNetBlobInputThread::run(void)
{
    ODL_OBJENTER(); //####
    try
    {
        for ( ; ! isStopping(); )
        {
            ConsumeSomeTime();
        }
#if (! defined(MpM_BuildDummyServices))
        if (_client)
        {
            _client->SetDataCallback(NULL, NULL);
        }
#endif // ! defined(MpM_BuildDummyServices)
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // NatNetBlobInputThread::run

void
NatNetBlobInputThread::sendMessage(const char * message,
                                   const size_t length)
{
    ODL_OBJENTER(); //####
    ODL_S1("message = ", message); //####

    if (_outChannel)
    {
        void *          rawString = static_cast<void *>(const_cast<char *>(message));
        yarp::os::Value blobValue(rawString, static_cast<int>(length));

        _messageBottle.clear();
        _messageBottle.add(blobValue);
        if (! _outChannel->write(_messageBottle))
        {
            ODL_LOG("(! _outChannel->write(_messageBottle))"); //####
#if defined(MpM_StallOnSendProblem)
            Stall();
#endif // defined(MpM_StallOnSendProblem)
        }
    }
    ODL_OBJEXIT(); //####
} // NatNetBlobInputThread::sendMessage

void
NatNetBlobInputThread::setScale(const double newScale)
{
    ODL_OBJENTER(); //####
    ODL_D1("newScale = ", newScale); //####
    _translationScale = newScale;
    ODL_OBJEXIT(); //####
} // NatNetBlobInputThread::setScale

bool
NatNetBlobInputThread::threadInit(void)
{
    ODL_OBJENTER(); //####
    bool result = true;

    try
    {
#if (! defined(MpM_BuildDummyServices))
        _client = new NatNetClient(NATNET_CONNECTION_MODE_);
        ODL_P1("_client <- ", _client); //####
        if (_client)
        {
            const char * theServerAddress = _address.c_str();

            if (theServerAddress && (IPADDRESS_BUFFER_SIZE > (strlen(theServerAddress) + 5)))
            {
                strcpy_s(_serverIPAddress, sizeof(_serverIPAddress) - 1, theServerAddress);
            }
            else
            {
                strcpy_s(_serverIPAddress, sizeof(_serverIPAddress) - 1, "");
            }
            _client->SetMessageCallback(messageReceived);
            _client->SetVerbosityLevel(Verbosity_Debug);
            _client->SetDataCallback(dataReceived, this);
            int retCode = _client->Initialize(_clientIPAddress, _serverIPAddress, _commandPort,
                                              _dataPort);

            if (ErrorCode_OK == retCode)
            {
                sServerDescription theServer;

                memset(&theServer, 0, sizeof(theServer));
                _client->GetServerDescription(&theServer);
                if (! theServer.HostPresent)
                {
                    ODL_LOG("(! theServer.HostPresent)"); //####
                    std::cerr << "Natural Point NatNet service host not present." << std::endl;
                    result = false;
                }
            }
            else
            {
                ODL_LOG("(_client->Initialize(NULL, const_cast<char *>(_address.c_str()), " //####
                        "_commandPort, _dataPort))"); //####
                std::cerr << "Initialization problem with Natural Point NatNet service: ";
                switch (retCode)
                {
                    case ErrorCode_External :
                        std::cerr << "external issue.";
                        break;

                    case ErrorCode_Internal :
                        std::cerr << "internal issue.";
                        break;

                    case ErrorCode_Network :
                        std::cerr << "network issue.";
                        break;

                    default :
                        std::cerr << "unknown issue.";
                        break;

                }
                std::cerr << std::endl;
                result = false;
            }
        }
        else
        {
            result = false;
        }
#endif // ! defined(MpM_BuildDummyServices)
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        result = false;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // NatNetBlobInputThread::threadInit

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
