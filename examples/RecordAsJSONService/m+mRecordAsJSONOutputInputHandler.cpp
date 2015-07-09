//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRecordAsJSONOutputInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the input channel input handler used by the Record As JSON
//              output service.
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
//  Created:    2014-11-26
//
//--------------------------------------------------------------------------------------------------

#include "m+mRecordAsJSONOutputInputHandler.h"

#include <m+m/m+mUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the input channel input handler used by the Record As JSON output
 service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::RecordAsJSON;

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
 @param outFile The file to be written to.
 @param inputValue The value to be processed. */
static void processValue(FILE *                   outFile,
                         const yarp::os::Value & inputValue);

/*! @brief Convert a YARP string into a JSON string.
 @param outFile The file to be written to.
 @param inputString The string to be processed. */
static void processString(FILE *             outFile,
                          const YarpString & inputString)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outFile = ", outFile, "inputString = ", &inputString); //####
    
    fputc('"', outFile);
    for (size_t ii = 0, mm = inputString.length(); mm > ii; ++ii)
    {
        char aChar = inputString[ii];
        
        switch (aChar)
        {
            case '\\' :
            case '"' :
            case '/' :
                fputc(kEscapeChar, outFile);
                fputc(aChar, outFile);
                break;
                
            case '\b' :
                fputc(kEscapeChar, outFile);
                fputc('b', outFile);
                break;
                
            case '\f' :
                fputc(kEscapeChar, outFile);
                fputc('f', outFile);
                break;
                
            case '\n' :
                fputc(kEscapeChar, outFile);
                fputc('n', outFile);
                break;
                
            case '\r' :
                fputc(kEscapeChar, outFile);
                fputc('r', outFile);
                break;
                
            case '\t' :
                fputc(kEscapeChar, outFile);
                fputc('t', outFile);
                break;
                
            default :
                fputc(aChar, outFile);
                break;
                
        }
    }
    fputc('"', outFile);
    OD_LOG_EXIT(); //####
} // processString

/*! @brief Convert a YARP dictionary into a JSON object.
 @param outFile The file to be written to.
 @param inputDictionary The dictionary to be processed. */
static void processDictionary(FILE *                     outFile,
                              const yarp::os::Property & inputDictionary)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outFile = ", outFile, "inputDictionary = ", &inputDictionary); //####
    yarp::os::Bottle asList(inputDictionary.toString());
    
    // A dictionary converted to a string is a list of two-element lists, with the key as the first
    // entry and the value as the second.
    fputs("{ ", outFile);
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
                    fputs(", ", outFile);
                }
                processString(outFile, entryAsList->get(0).toString());
                fputs(" : ", outFile);
                processValue(outFile, entryAsList->get(1));
            }
        }
    }
    fputs(" }", outFile);
    OD_LOG_EXIT(); //####
} // processDictionary

/*! @brief Convert a YARP list into a JSON array.
 @param outFile The file to be written to.
 @param inputList The list to be processed. */
static void processList(FILE *                   outFile,
                        const yarp::os::Bottle & inputList)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outFile = ", outFile, "inputList = ", &inputList); //####

    fputs("( ", outFile);
    for (int ii = 0, mm = inputList.size(); mm > ii; ++ii)
    {
        yarp::os::Value aValue(inputList.get(ii));

        if (0 < ii)
        {
            fputs(", ", outFile);
        }
        processValue(outFile, aValue);
    }
    fputs(" )", outFile);
    OD_LOG_EXIT(); //####
} // processList

static void processValue(FILE *                   outFile,
                         const yarp::os::Value & inputValue)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outFile = ", outFile, "inputValue = ", &inputValue); //####
    
    if (inputValue.isBool())
    {
        bool value = inputValue.asBool();
        
        fputs(value ? "true" : "false", outFile);
    }
    else if (inputValue.isInt())
    {
        int value = inputValue.asInt();
        
        fprintf(outFile, "%d", value);
    }
    else if (inputValue.isString())
    {
        YarpString value = inputValue.asString();
        
        processString(outFile, value);
    }
    else if (inputValue.isDouble())
    {
        double value = inputValue.asDouble();
        
        fprintf(outFile, "%g", value);
    }
    else if (inputValue.isDict())
    {
        yarp::os::Property * value = inputValue.asDict();
        
        if (value)
        {
            processDictionary(outFile, *value);
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
                processDictionary(outFile, asDict);
            }
            else
            {
                processList(outFile, *value);
            }
        }
    }
    else
    {
        // We don't know what to do with this...
        fputs("null", outFile);
    }
    OD_LOG_EXIT(); //####
} // processValue

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

RecordAsJSONOutputInputHandler::RecordAsJSONOutputInputHandler(void) :
    inherited(), _outFile(nullptr)
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // RecordAsJSONOutputInputHandler::RecordAsJSONOutputInputHandler

RecordAsJSONOutputInputHandler::~RecordAsJSONOutputInputHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // RecordAsJSONInputHandler::~RecordAsJSONInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool RecordAsJSONOutputInputHandler::handleInput(const yarp::os::Bottle &     input,
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
        if (_outFile)
        {
            OD_LOG("(_outFile)"); //####
            int mm = input.size();
            
            if (0 < mm)
            {
                int64_t now = Utilities::GetCurrentTimeInMilliseconds();
                
                fprintf(_outFile, "{ \"time\" : %lld, \"value\" : ", now);
                if (1 == mm)
                {
                    processValue(_outFile, input.get(0));
                }
                else if (1 < mm)
                {
                    processList(_outFile, input);
                }
                fputs(" }\n", _outFile);
                fflush(_outFile);
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
} // RecordAsJSONOutputInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void RecordAsJSONOutputInputHandler::setFile(FILE * outFile)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("outFile = ", outFile); //####
    _outFile = outFile;
    OD_LOG_OBJEXIT(); //####
} // RecordAsJSONOutputInputHandler::setFile

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
