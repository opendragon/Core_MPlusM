//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCommonLispInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the input channel input handler used by the Common Lisp
//              input / output service.
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
//  Created:    2015-08-05
//
//--------------------------------------------------------------------------------------------------

#include "m+mCommonLispInputHandler.h"
#include "m+mCommonLispService.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the input channel input handler used by the Common Lisp
 input / output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::CommonLisp;
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

#if 0
/*! @brief Convert a YARP value into a Common Lisp object.
 @param jct The Common Lisp engine context.
 @param theData The output object.
 @param inputValue The value to be processed. */
static void convertValue(JSContext *             jct,
                         JS::MutableHandleValue  theData,
                         const yarp::os::Value & inputValue);

/*! @brief Convert a YARP dictionary into a Common Lisp object.
 @param jct The %CommonLisp engine context.
 @param theData The output object.
 @param inputValue The value to be processed.
 @param inputAsList The input dictionary as a list. */
static void convertDictionary(JSContext *                jct,
                              JS::MutableHandleValue     theData,
                              const yarp::os::Property & inputValue,
                              const yarp::os::Bottle &   inputAsList)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("jct = ", jct, "inputValue = ", &inputValue, "inputAsList = ", &inputAsList); //####
    JS::RootedObject empty(jct);
    JSObject *       valueObject = JS_NewObject(jct, NULL);

    if (valueObject)
    {
        JS::RootedObject objectRooted(jct);
        JS::RootedValue  anElement(jct);
        
        objectRooted = valueObject;
        for (int ii = 0, mm = inputAsList.size(); mm > ii; ++ii)
        {
            yarp::os::Value anEntry(inputAsList.get(ii));
            
            if (anEntry.isList())
            {
                yarp::os::Bottle * entryAsList = anEntry.asList();
                
                if (entryAsList && (2 == entryAsList->size()))
                {
                    yarp::os::Value aValue(entryAsList->get(1));

                    convertValue(jct, &anElement, aValue);
                    JS_SetProperty(jct, objectRooted, entryAsList->get(0).toString().c_str(),
                                   anElement);
                }
            }
        }
        theData.setObject(*valueObject);
    }
    OD_LOG_EXIT(); //####
} // convertDictionary

// convertList
/*! @brief Convert a YARP list into a Common Lisp object.
 @param jct The %CommonLisp engine context.
 @param theData The output object.
 @param inputValue The value to be processed. */
static void convertList(JSContext *              jct,
                        JS::MutableHandleValue   theData,
                        const yarp::os::Bottle & inputValue)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "inputValue = ", &inputValue); //####
    JSObject * valueArray = JS_NewArrayObject(jct, 0);
    
    if (valueArray)
    {
        JS::RootedObject arrayRooted(jct);
        JS::RootedValue  anElement(jct);
        JS::RootedId     aRootedId(jct);

        arrayRooted = valueArray;
        for (int ii = 0, mm = inputValue.size(); mm > ii; ++ii)
        {
            yarp::os::Value aValue(inputValue.get(ii));
            
            convertValue(jct, &anElement, aValue);
            if (JS_IndexToId(jct, ii, &aRootedId))
            {
                JS_SetPropertyById(jct, arrayRooted, aRootedId, anElement);
            }
        }
        theData.setObject(*valueArray);
    }
    OD_LOG_EXIT(); //####
} // convertList

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
        YarpString value = inputValue.asString();
        JSString * aString = JS_NewStringCopyZ(jct, value.c_str());
        
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
            yarp::os::Bottle asList(value->toString());

            convertDictionary(jct, theData, *value, asList);
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
                convertDictionary(jct, theData, asDict, *value);
            }
            else
            {
                convertList(jct, theData, *value);
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
 @param jct The %CommonLisp engine context.
 @param aBottle The bottle to be filled.
 @param theData The value to be sent. */
static void createValueFromBottle(JSContext *              jct,
                                  const yarp::os::Bottle & aBottle,
                                  JS::MutableHandleValue   theData)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "aBottle = ", &aBottle); //####
//    cerr << "'" << aBottle.toString().c_str() << "'" << endl << endl;
    convertList(jct, theData, aBottle);
    OD_LOG_EXIT(); //####
} // createValueFromBottle
#endif//0

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

CommonLispInputHandler::CommonLispInputHandler(CommonLispService * owner,
                                               const size_t        slotNumber,
                                               cl_object           handlerFunc) :
    inherited(), _owner(owner), _handlerFunc(handlerFunc), _slotNumber(slotNumber), _active(false)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("owner = ", owner, "handlerFunc = ", handlerFunc); //####
    OD_LOG_L1("slotNumber = ", slotNumber); //####
    OD_LOG_EXIT_P(this); //####
} // CommonLispInputHandler::CommonLispInputHandler

CommonLispInputHandler::~CommonLispInputHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // CommonLispInputHandler::~CommonLispInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
DEFINE_HANDLE_INPUT_(CommonLispInputHandler)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(senderChannel,replyMechanism)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    OD_LOG_OBJENTER(); //####
    OD_LOG_S2s("senderChannel = ", senderChannel, "got ", input.toString()); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    OD_LOG_L1("numBytes = ", numBytes); //####
    bool result = true;
    
    try
    {
        if (_active && _owner && _handlerFunc)
        {
            // need to pass port number and incoming data to function; ignore result
            cl_object incoming = ECL_NIL; //!!!!

            cl_funcall(1, _handlerFunc, ecl_make_fixnum(_slotNumber), incoming);

#if 0
            JSContext * jct = _owner->getContext();
            
            if (jct)
            {
                JS::RootedValue argValue(jct);
                JS::Value       slotNumberValue;

                slotNumberValue.setInt32(static_cast<int32_t>(_slotNumber));
                createValueFromBottle(jct, input, &argValue);
                JS::AutoValueVector funcArgs(jct);
                JS::RootedValue     funcResult(jct);
                
                result = false;
                funcArgs.append(slotNumberValue);
                funcArgs.append(argValue);
                JS_BeginRequest(jct);
                if (JS_CallFunctionValue(jct, _owner->getGlobal(), _handlerFunc, funcArgs,
                                         &funcResult))
                {
                    // We don't care about the function result, as it's supposed to just write to
                    // the outlet stream(s).
                    result = true;
                }
                else
                {
                    OD_LOG("! (JS_CallFunctionValue(jct, _owner->getGlobal(), _handlerFunc, " //####
                           "funcArgs, &funcResult))"); //####
                    JS::RootedValue exc(jct);
                    
                    if (JS_GetPendingException(jct, &exc))
                    {
                        JS_ClearPendingException(jct);
                        std::stringstream buff;
                        YarpString        message("Exception occurred while executing "
                                                  "handler function for inlet ");
                        
                        buff << _slotNumber;
                        message += buff.str();
                        message += ".";
#if MAC_OR_LINUX_
                        GetLogger().fail(message.c_str());
#else // ! MAC_OR_LINUX_
                        cerr << message.c_str() << endl;
#endif // ! MAC_OR_LINUX_
                    }
                }
                JS_EndRequest(jct);
            }
#endif//0
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // CommonLispInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
