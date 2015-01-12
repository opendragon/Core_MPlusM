//--------------------------------------------------------------------------------------------------
//
//  File:       M+MJavaScriptInputHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the input channel input handler used by the JavaScript
//              input / output service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by HPlus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2015-01-05
//
//--------------------------------------------------------------------------------------------------

#include "M+MJavaScriptInputHandler.h"
#include "M+MJavaScriptService.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the input channel input handler used by the %JavaScript
 input / output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::JavaScript;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Convert a YARP value into a %JavaScript object.
 @param jct The %JavaScript engine context.
 @param theData The output object.
 @param inputValue The value to be processed. */
static void convertValue(JSContext *             jct,
                         JS::MutableHandleValue  theData,
                         const yarp::os::Value & inputValue);






static void convertValue(JSContext *             jct,
                         JS::MutableHandleValue  theData,
                         const yarp::os::Value & inputValue)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "inputValue = ", &inputValue); //####
    if (inputValue.isBool())
    {
        bool value = inputValue.asBool();
        
        theData.setBoolean(value);
    }
    else if (inputValue.isInt())
    {
        int value = inputValue.asInt();
        
        theData.setInt32(value);
    }
    else if (inputValue.isString())
    {
        yarp::os::ConstString value = inputValue.asString();
        JSString *            aString = JS_NewStringCopyZ(jct, value.c_str());
        
        if (aString)
        {
            theData.setString(aString);
        }
    }
    else if (inputValue.isDouble())
    {
        double value = inputValue.asDouble();
        
        theData.setDouble(value);
    }
    else if (inputValue.isDict())
    {
        yarp::os::Property * value = inputValue.asDict();
        
        if (value)
        {
            std::cout << "inside dictionary" << std::endl;
//            processDictionary(outFile, *value);
        }
    }
    else if (inputValue.isList())
    {
        yarp::os::Bottle * value = inputValue.asList();
        
        if (value)
        {
            std::cout << "inside list" << std::endl;
            yarp::os::Property asDict;
            
            if (ListIsReallyDictionary(*value, asDict))
            {
                std::cout << "really a dictionary" << std::endl;
//                processDictionary(outFile, asDict);
            }
            else
            {
//                processList(outFile, *value);
            }
        }
    }
    else
    {
        // We don't know what to do with this...
        theData.setNull();
    }
    OD_LOG_EXIT(); //####
} // convertValue

/*! @brief Fill a bottle with the contents of an object.
 @param jct The %JavaScript engine context.
 @param aBottle The bottle to be filled.
 @param theData The value to be sent. */
static void createValueFromBottle(JSContext *              jct,
                                  const yarp::os::Bottle & aBottle,
                                  JS::MutableHandleValue   theData)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "aBottle = ", aBottle); //####
    int  mm = aBottle.size();
    
    std::cout << "'" << aBottle.toString().c_str() << "'" << std::endl << std::endl;
    
    if (1 == mm)
    {
        convertValue(jct, theData, aBottle.get(0));
    }
    else if (1 < mm)
    {
        std::cout << "list" << std::endl;
//        convertList(theData, aBottle);
    }
    else
    {
        theData.setNull();
    }
    OD_LOG_EXIT(); //####
} // createValueFromBottle

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

JavaScriptInputHandler::JavaScriptInputHandler(JavaScriptService * owner,
                                               const size_t        slotNumber,
                                               JS::HandleValue &   handlerFunc) :
    inherited(), _owner(owner), _handlerFunc(handlerFunc), _slotNumber(slotNumber), _active(false)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("owner = ", owner, "handlerFunc = ", &handlerFunc); //####
    OD_LOG_L1("slotNumber = ", slotNumber); //####
    OD_LOG_EXIT_P(this); //####
} // JavaScriptInputHandler::JavaScriptInputHandler

JavaScriptInputHandler::~JavaScriptInputHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // JavaScriptInputHandler::~JavaScriptInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool JavaScriptInputHandler::handleInput(const yarp::os::Bottle &      input,
                                         const yarp::os::ConstString & senderChannel,
                                         yarp::os::ConnectionWriter *  replyMechanism,
                                         const size_t                  numBytes)
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
        if (_active && _owner)
        {
            JSContext * jct = _owner->getContext();
            
            if (jct)
            {
                std::cout << __LINE__ << std::endl;
                JS::RootedValue argValue(jct);
                JS::Value       slotNumberValue;

                slotNumberValue.setInt32(_slotNumber);
                createValueFromBottle(jct, input, &argValue);
//#if 0
                PrintJavaScriptValue(std::cout, jct, "incoming = ", argValue, 0);
                std::cout << std::endl;
//#endif//0
                JS::AutoValueVector funcArgs(jct);
                JS::RootedValue     funcResult(jct);
                
                result = false;
                std::cout << __LINE__ << std::endl;
                funcArgs.append(slotNumberValue);
                funcArgs.append(argValue);
                JS_BeginRequest(jct);
                std::cout << __LINE__ << std::endl;
                if (JS_CallFunctionValue(jct, _owner->getGlobal(), _handlerFunc, funcArgs,
                                         &funcResult))
                {
                    std::cout << __LINE__ << " OK" << std::endl;
                    // We don't care about the function result, as it's supposed to just write to
                    // the outlet stream(s).
                    result = true;
                }
                else
                {
                    std::cout << __LINE__ << std::endl;
                    OD_LOG("! (JS_CallFunctionValue(jct, _owner->getGlobal(), _handlerFunc, " //####
                           "funcArgs, &funcResult))"); //####
                    JS::RootedValue exc(jct);
                    
                    if (JS_GetPendingException(jct, &exc))
                    {
                        JS_ClearPendingException(jct);
                        std::stringstream     buff;
                        yarp::os::ConstString message("Exception occurred while executing "
                                                      "handler function for inlet ");
                        
                        buff << _slotNumber;
                        message += buff.str();
                        message += ".";
#if MAC_OR_LINUX_
                        GetLogger().fail(message.c_str());
#else // ! MAC_OR_LINUX_
                        std::cerr << message.c_str() << std::endl;
#endif // ! MAC_OR_LINUX_
                    }
                }
                JS_EndRequest(jct);
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
} // JavaScriptInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
