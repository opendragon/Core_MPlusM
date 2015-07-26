//--------------------------------------------------------------------------------------------------
//
//  File:       m+mSendToMQOutputInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the input handler used by the SendToMQ output service.
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
//  Created:    2015-07-26
//
//--------------------------------------------------------------------------------------------------

#include "m+mSendToMQOutputInputHandler.h"
#include "m+mSendToMQOutputService.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the input handler used by the %SendToMQ output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::SendToMQ;
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

/*! @brief Convert a YARP value into a JSON element.
 @param outBuffer The buffer to be written to.
 @param inputValue The value to be processed. */
#if defined(MpM_UseCustomStringBuffer)
static void processValue(Common::StringBuffer &  outBuffer,
                         const yarp::os::Value & inputValue);
#else // ! defined(MpM_UseCustomStringBuffer)
static void processValue(std::stringstream &     outBuffer,
                         const yarp::os::Value & inputValue);
#endif // ! defined(MpM_UseCustomStringBuffer)

/*! @brief Convert a YARP string into a JSON string.
 @param outBuffer The buffer to be written to.
 @param inputString The string to be processed. */
#if defined(MpM_UseCustomStringBuffer)
static void processString(Common::StringBuffer & outBuffer,
                          const YarpString &     inputString)
#else // ! defined(MpM_UseCustomStringBuffer)
static void processString(std::stringstream & outBuffer,
                          const YarpString &  inputString)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outBuffer = ", &outBuffer, "inputString = ", &inputString); //####
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addChar('"');
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << '"';
#endif // ! defined(MpM_UseCustomStringBuffer)
    for (size_t ii = 0, mm = inputString.length(); mm > ii; ++ii)
    {
        char aChar = inputString[ii];

        switch (aChar)
        {
            case '\\' :
            case '"' :
            case '/' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar(aChar);
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << aChar;
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;

            case '\b' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar('b');
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << 'b';
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;

            case '\f' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar('f');
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << 'f';
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;

            case '\n' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar('n');
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << 'n';
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;

            case '\r' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar('r');
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << 'r';
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;

            case '\t' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar('t');
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << 't';
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;

            default :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(aChar);
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << aChar;
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;

        }
    }
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addChar('"');
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << '"';
#endif // ! defined(MpM_UseCustomStringBuffer)
    OD_LOG_EXIT(); //####
} // processString

/*! @brief Convert a YARP dictionary into a JSON object.
 @param outBuffer The buffer to be written to.
 @param inputDictionary The dictionary to be processed. */
#if defined(MpM_UseCustomStringBuffer)
static void processDictionary(Common::StringBuffer &     outBuffer,
                              const yarp::os::Property & inputDictionary)
#else // ! defined(MpM_UseCustomStringBuffer)
static void processDictionary(std::stringstream &        outBuffer,
                              const yarp::os::Property & inputDictionary)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outBuffer = ", &outBuffer, "inputDictionary = ", &inputDictionary); //####
    yarp::os::Bottle asList(inputDictionary.toString());

    // A dictionary converted to a string is a list of two-element lists, with the key as the first
    // entry and the value as the second.
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addString("{ ");
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << "{ ";
#endif // ! defined(MpM_UseCustomStringBuffer)
    for (int ii = 0, mm = asList.size(); mm > ii; ++ii)
    {
        yarp::os::Value anEntry(asList.get(ii));

        if (anEntry.isList())
        {
            yarp::os::Bottle * entryAsList = anEntry.asList();

            if (entryAsList && (2 == entryAsList->size()))
            {
                if (0 < ii)
                {
#if defined(MpM_UseCustomStringBuffer)
                    outBuffer.addString(", ");
#else // ! defined(MpM_UseCustomStringBuffer)
                    outBuffer << ", ";
#endif // ! defined(MpM_UseCustomStringBuffer)
                }
                processString(outBuffer, entryAsList->get(0).toString());
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addString(" : ");
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << " : ";
#endif // ! defined(MpM_UseCustomStringBuffer)
                processValue(outBuffer, entryAsList->get(1));
            }
        }
    }
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addString(" }");
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << " }";
#endif // ! defined(MpM_UseCustomStringBuffer)
    OD_LOG_EXIT(); //####
} // processDictionary

/*! @brief Convert a YARP list into a JSON array.
 @param outBuffer The buffer to be written to.
 @param inputList The list to be processed. */
#if defined(MpM_UseCustomStringBuffer)
static void processList(Common::StringBuffer &   outBuffer,
                        const yarp::os::Bottle & inputList)
#else // ! defined(MpM_UseCustomStringBuffer)
static void processList(std::stringstream &      outBuffer,
                        const yarp::os::Bottle & inputList)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outBuffer = ", &outBuffer, "inputList = ", &inputList); //####
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addString("( ");
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << "( ";
#endif // ! defined(MpM_UseCustomStringBuffer)
    for (int ii = 0, mm = inputList.size(); mm > ii; ++ii)
    {
        yarp::os::Value aValue(inputList.get(ii));

        if (0 < ii)
        {
#if defined(MpM_UseCustomStringBuffer)
            outBuffer.addString(", ");
#else // ! defined(MpM_UseCustomStringBuffer)
            outBuffer << ", ";
#endif // ! defined(MpM_UseCustomStringBuffer)
        }
        processValue(outBuffer, aValue);
    }
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addString(" )");
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << " )";
#endif // ! defined(MpM_UseCustomStringBuffer)
    OD_LOG_EXIT(); //####
} // processList

#if defined(MpM_UseCustomStringBuffer)
static void processValue(Common::StringBuffer &  outBuffer,
                         const yarp::os::Value & inputValue)
#else // ! defined(MpM_UseCustomStringBuffer)
static void processValue(std::stringstream &     outBuffer,
                         const yarp::os::Value & inputValue)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outBuffer = ", &outBuffer, "inputValue = ", &inputValue); //####
    if (inputValue.isBool())
    {
        bool value = inputValue.asBool();

#if defined(MpM_UseCustomStringBuffer)
        outBuffer.addString(value ? "true" : "false");
#else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << (value ? "true" : "false");
#endif // ! defined(MpM_UseCustomStringBuffer)
    }
    else if (inputValue.isInt())
    {
        int value = inputValue.asInt();

#if defined(MpM_UseCustomStringBuffer)
        outBuffer.addLong(value);
#else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << value;
#endif // ! defined(MpM_UseCustomStringBuffer)
    }
    else if (inputValue.isString())
    {
        YarpString value = inputValue.asString();

        processString(outBuffer, value);
    }
    else if (inputValue.isDouble())
    {
        double value = inputValue.asDouble();

#if defined(MpM_UseCustomStringBuffer)
        outBuffer.addDouble(value);
#else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << value;
#endif // ! defined(MpM_UseCustomStringBuffer)
    }
    else if (inputValue.isDict())
    {
        yarp::os::Property * value = inputValue.asDict();

        if (value)
        {
            processDictionary(outBuffer, *value);
        }
    }
    else if (inputValue.isList())
    {
        yarp::os::Bottle * value = inputValue.asList();

        if (value)
        {
            yarp::os::Property asDict;

            if (ListIsReallyDictionary(*value, asDict))
            {
                processDictionary(outBuffer, asDict);
            }
            else
            {
                processList(outBuffer, *value);
            }
        }
    }
    else
    {
        // We don't know what to do with this...
#if defined(MpM_UseCustomStringBuffer)
        outBuffer.addString("null");
#else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << "null";
#endif // ! defined(MpM_UseCustomStringBuffer)
    }
    OD_LOG_EXIT(); //####
} // processValue

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

SendToMQOutputInputHandler::SendToMQOutputInputHandler(SendToMQOutputService & owner) :
    inherited(), _owner(owner), _outSocket(INVALID_SOCKET)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("owner = ", &owner); //####
    OD_LOG_EXIT_P(this); //####
} // SendToMQOutputInputHandler::SendToMQOutputInputHandler

SendToMQOutputInputHandler::~SendToMQOutputInputHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // SendToMQOutputInputHandler::~SendToMQOutputInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool SendToMQOutputInputHandler::handleInput(const yarp::os::Bottle &     input,
                                             const YarpString &           senderChannel,
                                             yarp::os::ConnectionWriter * replyMechanism,
                                             const size_t                 numBytes)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(senderChannel,replyMechanism)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER(); //####
    OD_LOG_S2s("senderChannel = ", senderChannel, "got ", input.toString()); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    OD_LOG_L1("numBytes = ", numBytes); //####
    bool result = true;
    
    try
    {
        if (_owner.isActive())
        {
            if (INVALID_SOCKET == _outSocket)
            {
                cerr << "invalid socket" << endl; //!!!!
            }
            else
            {
                int mm = input.size();

                if (0 < mm)
                {
#if (! defined(MpM_UseCustomStringBuffer))
                    std::stringstream outBuffer;
#endif // ! defined(MpM_UseCustomStringBuffer)
                    int64_t           now = Utilities::GetCurrentTimeInMilliseconds();

#if defined(MpM_UseCustomStringBuffer)
                    _outBuffer.reset();
                    _outBuffer.addString("{ \"time\" : ");
                    _outBuffer.addLong(now).addString(", \"value\" : ");
#else // ! defined(MpM_UseCustomStringBuffer)
                    outBuffer << "{ \"time\" : " << now << ", \"value\" : ";
#endif // ! defined(MpM_UseCustomStringBuffer)
                    if (1 == mm)
                    {
#if defined(MpM_UseCustomStringBuffer)
                        processValue(_outBuffer, input.get(0));
#else // ! defined(MpM_UseCustomStringBuffer)
                        processValue(outBuffer, input.get(0));
#endif // ! defined(MpM_UseCustomStringBuffer)
                    }
                    else if (1 < mm)
                    {
#if defined(MpM_UseCustomStringBuffer)
                        processList(_outBuffer, input);
#else // ! defined(MpM_UseCustomStringBuffer)
                        processList(outBuffer, input);
#endif // ! defined(MpM_UseCustomStringBuffer)
                    }
#if defined(MpM_UseCustomStringBuffer)
                    _outBuffer.addString(" }\n");
#else // ! defined(MpM_UseCustomStringBuffer)
                    outBuffer << " }\n";
#endif // ! defined(MpM_UseCustomStringBuffer)
                    const char * outString;
                    size_t       outLength;
#if (! defined(MpM_UseCustomStringBuffer))
                    std::string  buffAsString(outBuffer.str());
#endif // ! defined(MpM_UseCustomStringBuffer)

#if defined(MpM_UseCustomStringBuffer)
                    outString = _outBuffer.getString(outLength);
#else // ! defined(MpM_UseCustomStringBuffer)
                    outString = buffAsString.c_str();
                    outLength = buffAsString.length();
#endif // ! defined(MpM_UseCustomStringBuffer)
                    if (outString && outLength)
                    {
                        int retVal = send(_outSocket, outString, static_cast<int>(outLength), 0);

                        cerr << "send--> " << retVal << endl; //!!!!
                        if (0 > retVal)
                        {
                            _owner.deactivateConnection();
                        }
                    }
                }
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // SendToMQOutputInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void SendToMQOutputInputHandler::setSocket(const SOCKET outSocket)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_L1("outSocket = ", outSocket); //####
    _outSocket = outSocket;
    OD_LOG_OBJEXIT(); //####
} // SendToMQOutputInputHandler::setSocket

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
