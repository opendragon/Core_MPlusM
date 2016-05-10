//--------------------------------------------------------------------------------------------------
//
//  File:       m+mJavaScriptFilterService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the JavaScript filter service.
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
//  Created:    2015-01-05
//
//--------------------------------------------------------------------------------------------------

#include "m+mJavaScriptFilterService.hpp"

#include "m+mJavaScriptFilterInputHandler.hpp"
#include "m+mJavaScriptFilterRequests.hpp"
#include "m+mJavaScriptFilterThread.hpp"

#include <m+m/m+mEndpoint.hpp>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the %JavaScript filter service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::JavaScript;
using std::cerr;
using std::cout;
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

/*! @brief Convert a YARP value into a %JavaScript object.
 @param jct The %JavaScript engine context.
 @param theData The output object.
 @param inputValue The value to be processed. */
static void
convertValue(JSContext *             jct,
             JS::MutableHandleValue  theData,
             const yarp::os::Value & inputValue);

/*! @brief Convert a YARP dictionary into a %JavaScript object.
 @param jct The %JavaScript engine context.
 @param theData The output object.
 @param inputAsList The input dictionary as a list. */
static void
convertDictionary(JSContext *              jct,
                  JS::MutableHandleValue   theData,
                  const yarp::os::Bottle & inputAsList)
{
    ODL_ENTER(); //####
    ODL_P3("jct = ", jct, "theData = ", &theData, "inputAsList = ", &inputAsList); //####
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
    ODL_EXIT(); //####
} // convertDictionary

/*! @brief Convert a YARP list into a %JavaScript object.
 @param jct The %JavaScript engine context.
 @param theData The output object.
 @param inputValue The value to be processed. */
static void
convertList(JSContext *              jct,
            JS::MutableHandleValue   theData,
            const yarp::os::Bottle & inputValue)
{
    ODL_ENTER(); //####
    ODL_P2("jct = ", jct, "inputValue = ", &inputValue); //####
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
    ODL_EXIT(); //####
} // convertList

static void
convertValue(JSContext *             jct,
             JS::MutableHandleValue  theData,
             const yarp::os::Value & inputValue)
{
    ODL_ENTER(); //####
    ODL_P2("jct = ", jct, "inputValue = ", &inputValue); //####
    if (inputValue.isBool())
    {
        theData.setBoolean(inputValue.asBool());
    }
    else if (inputValue.isInt())
    {
        theData.setInt32(inputValue.asInt());
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
        theData.setDouble(inputValue.asDouble());
    }
    else if (inputValue.isDict())
    {
        yarp::os::Property * value = inputValue.asDict();

        if (value)
        {
            yarp::os::Bottle asList(value->toString());

            convertDictionary(jct, theData, asList);
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
                convertDictionary(jct, theData, *value);
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
    ODL_EXIT(); //####
} // convertValue

/*! @brief Fill an object with the contents of a bottle.
 @param jct The %JavaScript engine context.
 @param aBottle The bottle to be used.
 @param theData The value to be filled. */
static void
createValueFromBottle(JSContext *              jct,
                      const yarp::os::Bottle & aBottle,
                      JS::MutableHandleValue   theData)
{
    ODL_ENTER(); //####
    ODL_P2("jct = ", jct, "aBottle = ", &aBottle); //####
//    cerr << "'" << aBottle.toString().c_str() << "'" << endl << endl;
    convertList(jct, theData, aBottle);
    ODL_EXIT(); //####
} // createValueFromBottle

/*! @brief Fill a bottle with the contents of an object.
 @param jct The %JavaScript engine context.
 @param aBottle The bottle to be filled.
 @param theData The value to be sent.
 @param topLevel @c true if this is the outermost list of an object. */
static void
fillBottleFromValue(JSContext *        jct,
                    yarp::os::Bottle & aBottle,
                    JS::Value          theData,
                    const bool         topLevel)
{
    ODL_ENTER(); //####
    ODL_P2("jct = ", jct, "aBottle = ", &aBottle); //####
    ODL_B1("topLevel = ", topLevel); //####
    JS::RootedValue asRootedValue(jct);

    asRootedValue = theData;
    if (theData.isBoolean())
    {
        aBottle.addInt(JS::ToBoolean(asRootedValue) ? 1 : 0);
    }
    else if (theData.isDouble())
    {
        double aValue;

        if (JS::ToNumber(jct, asRootedValue, &aValue))
        {
            aBottle.addDouble(aValue);
        }
    }
    else if (theData.isInt32())
    {
        int32_t aValue;

        if (JS::ToInt32(jct, asRootedValue, &aValue))
        {
            aBottle.addInt(aValue);
        }
    }
    else if (theData.isString())
    {
        JSString * asString = theData.toString();
        char *     asChars = JS_EncodeString(jct, asString);

        aBottle.addString(asChars);
        JS_free(jct, asChars);
    }
    else if (theData.isObject())
    {
        JS::RootedObject asObject(jct);

        if (JS_ValueToObject(jct, asRootedValue, &asObject))
        {
            bool processed = false;
#if (47 <= MOZJS_MAJOR_VERSION)
            bool isArray;
#endif // 47 <= MOZJS_MAJOR_VERSION

#if (47 <= MOZJS_MAJOR_VERSION)
            if (JS_IsArrayObject(jct, asObject, &isArray))
#else // 47 > MOZJS_MAJOR_VERSION
            if (JS_IsArrayObject(jct, asObject))
#endif // 47 > MOZJS_MAJOR_VERSION
            {
                uint32_t arrayLength;

                if (JS_GetArrayLength(jct, asObject, &arrayLength))
                {
                    // Treat as a list
                    if (topLevel)
                    {
                        for (uint32_t ii = 0; arrayLength > ii; ++ii)
                        {
                            JS::RootedValue anElement(jct);

                            if (JS_GetElement(jct, asObject, ii, &anElement))
                            {
                                fillBottleFromValue(jct, aBottle, anElement, false);
                            }
                        }
                    }
                    else
                    {
                        yarp::os::Bottle & innerList(aBottle.addList());

                        for (uint32_t ii = 0; arrayLength > ii; ++ii)
                        {
                            JS::RootedValue anElement(jct);

                            if (JS_GetElement(jct, asObject, ii, &anElement))
                            {
                                fillBottleFromValue(jct, innerList, anElement, false);
                            }
                        }

                    }
                    processed = true;
                }
            }
            if (! processed)
            {
                // Treat as a dictionary
                yarp::os::Property &     innerDict(aBottle.addDict());
#if (47 <= MOZJS_MAJOR_VERSION)
                JS::Rooted<JS::IdVector> ids(jct, JS::IdVector(jct));
#else // 47 > MOZJS_MAJOR_VERSION
                JS::AutoIdArray          ids(jct, JS_Enumerate(jct, asObject));
#endif // 47 > MOZJS_MAJOR_VERSION

#if (47 <= MOZJS_MAJOR_VERSION)
                if (JS_Enumerate(jct, asObject, &ids))
#else // 47 > MOZJS_MAJOR_VERSION
                // Note that only operator! is defined, so we need to do a 'double-negative'.
                if (!! ids)
#endif // 47 > MOZJS_MAJOR_VERSION
                {
                    bool okSoFar = true;

                    for (size_t ii = 0, len = ids.length(); len > ii; ++ii)
                    {
                        JS::RootedValue key(jct);

                        if (JS_IdToValue(jct, ids[ii], &key))
                        {
                            JS::RootedValue key(jct);
                            JS::RootedValue result(jct);
                            JS::RootedId    aRootedId(jct);

                            aRootedId = ids[ii];
                            if (JS_IdToValue(jct, ids[ii], &key) &&
                                JS_GetPropertyById(jct, asObject, aRootedId, &result))
                            {
                                JSString *       keyString = key.toString();
                                char *           keyAsChars = JS_EncodeString(jct, keyString);
                                YarpString       keyToUse(keyAsChars);
                                yarp::os::Bottle convertedResult;

                                JS_free(jct, keyAsChars);
                                fillBottleFromValue(jct, convertedResult, result, false);
                                if (1 == convertedResult.size())
                                {
                                    yarp::os::Value anElement(convertedResult.get(0));

                                    if (anElement.isInt())
                                    {
                                        innerDict.put(keyToUse, anElement.asInt());
                                    }
                                    else if (anElement.isDouble())
                                    {
                                        innerDict.put(keyToUse, anElement.asDouble());
                                    }
                                    else if (anElement.isString())
                                    {
                                        innerDict.put(keyToUse, anElement.asString());
                                    }
                                    else
                                    {
                                        innerDict.put(keyToUse, anElement);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    ODL_EXIT(); //####
} // fillBottleFromValue

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

JavaScriptFilterService::JavaScriptFilterService(const Utilities::DescriptorVector & argumentList,
                                                 JSContext *                         context,
                                                 JS::RootedObject &                  global,
                                                 const YarpString &                  launchPath,
                                                 const int                           argc,
                                                 char * *                            argv,
                                                 const YarpString &                  tag,
                                                 const YarpString &                  description,
                                                 const Common::ChannelVector &
                                                                            loadedInletDescriptions,
                                                 const Common::ChannelVector &
                                                                        loadedOutletDescriptions,
                                                 const JS::AutoValueVector &
                                                                                loadedInletHandlers,
                                                 const JS::RootedValue &
                                                                            loadedStartingFunction,
                                                 const JS::RootedValue &
                                                                            loadedStoppingFunction,
                                                 const bool                          sawThread,
                                                 const JS::RootedValue &
                                                                            loadedThreadFunction,
                                                 const double                        loadedInterval,
                                                 const YarpString &
                                                                                serviceEndpointName,
                                                 const YarpString &
                                                                                servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true, MpM_JAVASCRIPTFILTER_CANONICAL_NAME_,
              description, "", serviceEndpointName, servicePortNumber), _inletHandlers(context),
    _inHandlers(), _generator(NULL), _context(context), _global(global),
    _loadedInletDescriptions(loadedInletDescriptions),
    _loadedOutletDescriptions(loadedOutletDescriptions), _goAhead(0), _staller(1),
    _scriptStartingFunc(context), _scriptStoppingFunc(context), _scriptThreadFunc(context),
    _threadInterval(loadedInterval), _mostRecentSlot(0), _isThreaded(sawThread)
{
    ODL_ENTER(); //####
    ODL_P4("argumentList = ", &argumentList, "context = ", context, "global = ", &global, //####
           "argv = ", argv); //####
    ODL_P4("loadedInletDescriptions = ", &loadedInletDescriptions, //####
           "loadedOutletDescriptions = ", &loadedOutletDescriptions, //####
           "loadedInletHandlers = ", &loadedInletHandlers, "loadedStartingFunction = ", //####
           &loadedStartingFunction); //####
    ODL_P2("loadedStoppingFunction = ", &loadedStoppingFunction, //####
           "loadedThreadFunction = ", &loadedThreadFunction); //####
    ODL_LL1("argc = ", argc); //####
    ODL_S4s("launchPath = ", launchPath, "tag = ", tag, "description = ", description, //####
            "serviceEndpointName = ", serviceEndpointName); //####
    ODL_S1s("servicePortNumber = ", servicePortNumber); //####
    ODL_B1("sawThread = ", sawThread); //####
    ODL_D1("loadedInterval = ", loadedInterval); //####
    JS_SetContextPrivate(context, this);
    _inletHandlers.appendAll(loadedInletHandlers);
    _scriptStartingFunc = loadedStartingFunction;
    _scriptStoppingFunc = loadedStoppingFunction;
    _scriptThreadFunc = loadedThreadFunction;
    if (_isThreaded && (! _scriptThreadFunc.isNullOrUndefined()))
    {
        ODL_LOG("(_isThreaded && (! _scriptThreadFunc.isNullOrUndefined()))"); //####
        setNeedsIdle();
    }
    else if (0 < _inletHandlers.length())
    {
        ODL_LOG("(0 < _inletHandlers.length())"); //####
        setNeedsIdle();
    }
    ODL_EXIT_P(this); //####
} // JavaScriptFilterService::JavaScriptFilterService

JavaScriptFilterService::~JavaScriptFilterService(void)
{
    ODL_OBJENTER(); //####
    stopStreams();
    releaseHandlers();
    ODL_OBJEXIT(); //####
} // JavaScriptFilterService::~JavaScriptFilterService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool
JavaScriptFilterService::configure(const yarp::os::Bottle & details)
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(details)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = false;

    try
    {
        // Check if the script is happy.
        if (_context && (! _scriptStartingFunc.isNullOrUndefined()))
        {
            JS::AutoValueVector funcArgs(_context);
            JS::RootedValue     funcResult(_context);

            JS_BeginRequest(_context);
            if (JS_CallFunctionValue(_context, _global, _scriptStartingFunc, funcArgs, &funcResult))
            {
                // Check if we got a result of 'true' - if not, there was a problem and we should
                // report it.
                if (funcResult.isBoolean())
                {
                    if (funcResult.toBoolean())
                    {
                        result = true;
                    }
                    else
                    {
                        // Script rejected starting, but gave no reason.
                        cout << "Could not configure -> unknown reason." << endl;
                    }
                }
                else
                {
                    // Script rejected starting.
                    JSString * reason = funcResult.toString();
                    char *     asChars = JS_EncodeString(_context, reason);

                    cout << "Could not configure -> " << asChars << "." << endl;
                    JS_free(_context, asChars);
                }
            }
            else
            {
                ODL_LOG("! (JS_CallFunctionValue(_context, _global, _scriptStartingFunc, " //####
                        "funcArgs, &funcResult))"); //####
                JS::RootedValue exc(_context);

                if (JS_GetPendingException(_context, &exc))
                {
                    JS_ClearPendingException(_context);
                    MpM_FAIL_("Exception occurred while executing scriptStarting function.");
                }
            }
            JS_EndRequest(_context);
        }
        else
        {
            result = true;
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // JavaScriptFilterService::configure
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
JavaScriptFilterService::disableMetrics(void)
{
    ODL_OBJENTER(); //####
    inherited::disableMetrics();
    for (HandlerVector::const_iterator walker(_inHandlers.begin()); _inHandlers.end() != walker;
         ++walker)
    {
        JavaScriptFilterInputHandler * aHandler = *walker;

        if (aHandler)
        {
            aHandler->disableMetrics();
        }
    }
    ODL_OBJEXIT(); //####
} // JavaScriptFilterService::disableMetrics

void
JavaScriptFilterService::doIdle(void)
{
    ODL_OBJENTER(); //####
    if (isActive())
    {
        ODL_LOG("(isActive())"); //####
        if (_goAhead.check())
        {
            ODL_LOG("(_goAhead.check())"); //####
            if (_scriptThreadFunc.isNullOrUndefined())
            {
                ODL_LOG("(_scriptThreadFunc.isNullOrUndefined())"); //####
                // We have a request from an input handler.
                if (_inHandlers.size() > _mostRecentSlot)
                {
                    ODL_LOG("(getInletCount() > _mostRecentSlot)"); //####
                    JS::HandleValue                handlerFunc = _inletHandlers[_mostRecentSlot];
                    JavaScriptFilterInputHandler * aHandler = _inHandlers.at(_mostRecentSlot);

                    if (aHandler && (! handlerFunc.isNullOrUndefined()))
                    {
                        ODL_LOG("(aHandler && (! handlerFunc.isNullOrUndefined()))"); //####
                        JS::RootedValue     argValue(_context);
                        JS::Value           slotNumberValue;
                        JS::AutoValueVector funcArgs(_context);
                        JS::RootedValue     funcResult(_context);

                        slotNumberValue.setInt32(static_cast<int32_t>(_mostRecentSlot));
                        createValueFromBottle(_context, aHandler->getReceivedData(), &argValue);
                        funcArgs.append(slotNumberValue);
                        funcArgs.append(argValue);
                        JS_BeginRequest(_context);
                        if (JS_CallFunctionValue(_context, _global, handlerFunc, funcArgs,
                                                 &funcResult))
                        {
                            // We don't care about the function result, as it's supposed to just
                            // write to the outlet stream(s).
                        }
                        else
                        {
                            ODL_LOG("! (JS_CallFunctionValue(_context, _global, " //####
                                    "handlerFunc, funcArgs, &funcResult))"); //####
                            JS::RootedValue exc(_context);

                            if (JS_GetPendingException(_context, &exc))
                            {
                                JS_ClearPendingException(_context);
                                std::stringstream buff;
                                YarpString        message("Exception occurred while executing "
                                                          "handler function for inlet ");

                                buff << _mostRecentSlot;
                                message += buff.str();
                                message += ".";
                                MpM_FAIL_(message.c_str());
                            }
                        }
                        JS_EndRequest(_context);
                    }
                }
                _staller.post();
            }
            else
            {
                ODL_LOG("! (_scriptThreadFunc.isNullOrUndefined())"); //####
                try
                {
                    JS::AutoValueVector funcArgs(_context);
                    JS::RootedValue     funcResult(_context);

                    JS_BeginRequest(_context);
                    if (JS_CallFunctionValue(_context, _global, _scriptThreadFunc, funcArgs,
                                             &funcResult))
                    {
                        ODL_LOG("(JS_CallFunctionValue(_context, _global, " //####
                                "_scriptThreadFunc, funcArgs, &funcResult))"); //####
                        // We don't care about the function result, as it's supposed to just perform
                        // an iteration of the thread.
                    }
                    else
                    {
                        ODL_LOG("! (JS_CallFunctionValue(_context, _global, " //####
                                "_scriptThreadFunc, funcArgs, &funcResult))"); //####
                        JS::RootedValue exc(_context);

                        if (JS_GetPendingException(_context, &exc))
                        {
                            ODL_LOG("(JS_GetPendingException(_context, &exc))"); //####
                            JS_ClearPendingException(_context);
                            MpM_FAIL_("Exception occurred while executing the scriptThread "
                                      "function.");
                        }
                    }
                    JS_EndRequest(_context);
                }
                catch (...)
                {
                    ODL_LOG("Exception caught"); //####
                    throw;
                }
            }
        }
    }
    ODL_OBJEXIT(); //####
} // JavaScriptFilterService::doIdle

void
JavaScriptFilterService::enableMetrics(void)
{
    ODL_OBJENTER(); //####
    inherited::enableMetrics();
    for (HandlerVector::const_iterator walker(_inHandlers.begin()); _inHandlers.end() != walker;
         ++walker)
    {
        JavaScriptFilterInputHandler * aHandler = *walker;

        if (aHandler)
        {
            aHandler->enableMetrics();
        }
    }
    ODL_OBJEXIT(); //####
} // JavaScriptFilterService::enableMetrics

void
JavaScriptFilterService::releaseHandlers(void)
{
    ODL_OBJENTER(); //####
    if (0 < _inHandlers.size())
    {
        for (HandlerVector::const_iterator walker(_inHandlers.begin()); _inHandlers.end() != walker;
             ++walker)
        {
            JavaScriptFilterInputHandler * aHandler = *walker;

            if (aHandler)
            {
                ODL_P1("aHandler = ", aHandler); //####
                delete aHandler;
            }
        }
        _inHandlers.clear();
    }
    ODL_OBJEXIT(); //####
} // JavaScriptFilterService::releaseHandlers

bool
JavaScriptFilterService::sendToChannel(const int32_t channelSlot,
                                            JS::Value     theData)
{
    ODL_OBJENTER();
    ODL_L1("channelSlot = ", channelSlot); //####
    bool okSoFar = false;

    if ((0 <= channelSlot) && (channelSlot < static_cast<int32_t>(getOutletCount())))
    {
        Common::GeneralChannel * outChannel = getOutletStream(channelSlot);
        yarp::os::Bottle         outBottle;

        fillBottleFromValue(_context, outBottle, theData, true);
        if ((0 < outBottle.size()) && outChannel)
        {
            if (outChannel->writeBottle(outBottle))
            {
                okSoFar = true;
            }
            else
            {
                ODL_LOG("! (outChannel->writeBottle(message))"); //####
#if defined(MpM_StallOnSendProblem)
                Stall();
#endif // defined(MpM_StallOnSendProblem)
            }
        }
        else
        {
            // If there's nothing to write, or the channel is gone, continue as if everything is
            // fine.
            okSoFar = true;
        }
    }
    ODL_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // JavaScriptFilterService::sendToChannel

bool
JavaScriptFilterService::setUpStreamDescriptions(void)
{
    ODL_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");

    _inDescriptions.clear();
    for (ChannelVector::const_iterator walker(_loadedInletDescriptions.begin());
         _loadedInletDescriptions.end() != walker; ++walker)
    {
        description._portName = rootName + walker->_portName;
        description._portProtocol = walker->_portProtocol;
        description._protocolDescription = walker->_protocolDescription;
        _inDescriptions.push_back(description);
    }
    _outDescriptions.clear();
    for (ChannelVector::const_iterator walker(_loadedOutletDescriptions.begin());
         _loadedOutletDescriptions.end() != walker; ++walker)
    {
        description._portName = rootName + walker->_portName;
        description._portProtocol = walker->_portProtocol;
        description._protocolDescription = walker->_protocolDescription;
        _outDescriptions.push_back(description);
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // JavaScriptFilterService::setUpStreamDescriptions

void
JavaScriptFilterService::signalRunFunction(void)
{
    ODL_OBJENTER(); //####
    _goAhead.post();
    ODL_OBJEXIT(); //####
} // JavaScriptFilterService::signalRunFunction

void
JavaScriptFilterService::stallUntilIdle(const size_t slotNumber)
{
    ODL_OBJENTER(); //####
    ODL_LL1("slotNumber = ", slotNumber); //####
    _staller.wait();
    _mostRecentSlot = slotNumber;
    ODL_OBJEXIT(); //####
} // JavaScriptFilterService::stallUntilIdle

void
JavaScriptFilterService::startStreams(void)
{
    ODL_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_isThreaded)
            {
                _generator = new JavaScriptFilterThread(*this, _threadInterval);
                if (! _generator->start())
                {
                    ODL_LOG("(! _generator->start())"); //####
                    cerr << "Could not start auxiliary thread." << endl;
                    delete _generator;
                    _generator = NULL;
                }
            }
            else
            {
                releaseHandlers();
                for (size_t ii = 0, mm = getInletCount(); mm > ii; ++ii)
                {
                    JavaScriptFilterInputHandler * aHandler = new JavaScriptFilterInputHandler(this,
                                                                                               ii);

                    if (aHandler)
                    {
                        _inHandlers.push_back(aHandler);
                        aHandler->setChannel(getInletStream(ii));
                        getInletStream(ii)->setReader(*aHandler);
                        aHandler->activate();
                    }
                }
            }
            setActive();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // JavaScriptFilterService::startStreams

void
JavaScriptFilterService::stopStreams(void)
{
    ODL_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            if (_isThreaded)
            {
                if (_generator)
                {
                    _generator->stop();
                    for ( ; _generator->isRunning(); )
                    {
                        yarp::os::Time::delay(_threadInterval / IO_SERVICE_DELAY_FACTOR_);
                    }
                    delete _generator;
                    _generator = NULL;
                }
            }
            else
            {
                for (size_t ii = 0, mm = getInletCount(); mm > ii; ++ii)
                {
                    JavaScriptFilterInputHandler * aHandler = _inHandlers.at(ii);

                    if (aHandler)
                    {
                        aHandler->deactivate();
                    }
                }
            }
            clearActive();
            // Tell the script that we're done for now.
            if (_context && (! _scriptStoppingFunc.isNullOrUndefined()))
            {
                JS::AutoValueVector funcArgs(_context);
                JS::RootedValue     funcResult(_context);

                JS_BeginRequest(_context);
                if (JS_CallFunctionValue(_context, _global, _scriptStoppingFunc, funcArgs,
                                         &funcResult))
                {
                    // We don't care about the function result, as it's supposed to just tell the
                    // script that it can clean up.
                }
                else
                {
                    ODL_LOG("! (JS_CallFunctionValue(_context, _global, " //####
                            "_scriptStoppingFunc, funcArgs, &funcResult))"); //####
                    JS::RootedValue exc(_context);

                    if (JS_GetPendingException(_context, &exc))
                    {
                        JS_ClearPendingException(_context);
                        MpM_FAIL_("Exception occurred while executing scriptStopping function.");
                    }
                }
                JS_EndRequest(_context);
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // JavaScriptFilterService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

std::ostream &
JavaScript::PrintJavaScriptObject(std::ostream &     outStream,
                                  JSContext *        jct,
                                  JS::RootedObject & anObject,
                                  const int          depth)
{
    ODL_ENTER(); //####
    ODL_P2("jct = ", jct, "anObject = ", &anObject); //####
    ODL_L1("depth = ", depth); //####
#if (47 <= MOZJS_MAJOR_VERSION)
    JS::Rooted<JS::IdVector> ids(jct, JS::IdVector(jct));
#else // 47 > MOZJS_MAJOR_VERSION
    JS::AutoIdArray          ids(jct, JS_Enumerate(jct, anObject));
#endif // 47 > MOZJS_MAJOR_VERSION

#if (47 <= MOZJS_MAJOR_VERSION)
    if (JS_Enumerate(jct, anObject, &ids))
#else // 47 > MOZJS_MAJOR_VERSION
    // Note that only operator! is defined, so we need to do a 'double-negative'.
    if (!! ids)
#endif // 47 > MOZJS_MAJOR_VERSION
    {
        bool okSoFar = true;

        for (size_t ii = 0, len = ids.length(); (len > ii) && okSoFar; ++ii)
        {
            JS::RootedValue key(jct);

            if (JS_IdToValue(jct, ids[ii], &key))
            {
                PrintJavaScriptValue(outStream, jct, "id = ", key, depth);
            }
            else
            {
                okSoFar = false;
            }
            if (okSoFar)
            {
                JS::RootedValue result(jct);
                JS::RootedId    aRootedId(jct);

                aRootedId = ids[ii];
                if (JS_GetPropertyById(jct, anObject, aRootedId, &result))
                {
                    PrintJavaScriptValue(outStream, jct, ", property = ", result, 0);
                }
                else
                {
                    okSoFar = false;
                }
                if (okSoFar)
                {
                    if (result.isObject())
                    {
                        JS::RootedObject asObject(jct);

                        if (JS_ValueToObject(jct, result, &asObject))
                        {
#if (47 <= MOZJS_MAJOR_VERSION)
                            bool isArray;
#endif // 47 <= MOZJS_MAJOR_VERSION

#if (47 <= MOZJS_MAJOR_VERSION)
                            if (JS_IsArrayObject(jct, result, &isArray))
#else // 47 > MOZJS_MAJOR_VERSION
                                if (JS_IsArrayObject(jct, result))
#endif // 47 > MOZJS_MAJOR_VERSION
                                {
                                    outStream << "array";
                                    uint32_t arrayLength;

                                    if (JS_GetArrayLength(jct, asObject, &arrayLength))
                                    {
                                        outStream << ", size = " << arrayLength;
                                    }
                                }
                                else if (JS_ObjectIsFunction(jct, asObject))
                                {
                                    outStream << "function";
                                    JSFunction * asFunction = JS_ValueToFunction(jct, result);

                                    if (asFunction)
                                    {
                                        outStream << ", arity = " <<
                                                     JS_GetFunctionArity(asFunction);
                                        if (! JS::IsCallable(asObject))
                                        {
                                            outStream << ", not callable";
                                        }
                                    }
                                }
                                else
                                {
                                    outStream << "object";
                                }
                        }
                        else
                        {
                            okSoFar = false;
                        }
                        outStream << endl;
                        if (okSoFar)
                        {
                            PrintJavaScriptObject(outStream, jct, asObject, depth + 1);
                        }
                    }
                    else
                    {
                        outStream << endl;
                    }
                }
            }
        }
    }
    ODL_EXIT_P(&outStream); //####
    return outStream;
} // JavaScript::PrintJavaScriptObject

std::ostream &
JavaScript::PrintJavaScriptValue(std::ostream &    outStream,
                                 JSContext *       jct,
                                 const char *      caption,
                                 JS::RootedValue & value,
                                 const int         depth)
{
    ODL_ENTER(); //####
    ODL_P3("outStream = ", &outStream, "jct = ", jct, "value = ", &value); //####
    ODL_S1("caption = ", caption); //####
    if (0 < depth)
    {
        outStream.width(depth);
        outStream << " ";
    }
    cout << caption;
    if (value.isString())
    {
        JSString * asString = value.toString();
        char *     asChars = JS_EncodeString(jct, asString);

        outStream << "string(" << asChars << ")";
        JS_free(jct, asChars);
    }
    else if (value.isObject())
    {
        // Objects will be processed separately.
    }
    else if (value.isInt32())
    {
        outStream << "int32(" << value.toInt32() << ")";
    }
    else if (value.isBoolean())
    {
        outStream << "boolean(" << (value.toBoolean() ? "true" : "false") << ")";
    }
    else if (value.isDouble())
    {
        outStream << "double(" << value.toDouble() << ")";
    }
    else if (value.isNullOrUndefined())
    {
        outStream << "null or undefined";
    }
    else
    {
        outStream << "other";
    }
    ODL_EXIT_P(&outStream); //####
    return outStream;
} // JavaScript::PrintJavaScriptValue
