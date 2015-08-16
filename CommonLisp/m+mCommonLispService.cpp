//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCommonLispService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Common Lisp input / output service.
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

#include "m+mCommonLispService.h"

#include "m+mCommonLispInputHandler.h"
#include "m+mCommonLispRequests.h"
#include "m+mCommonLispThread.h"

#include <m+m/m+mEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the Common Lisp input / output service. */
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
using std::cout;
using std::endl;

#if 0
#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Fill a bottle with the contents of an object.
 @param jct The Common Lisp engine context.
 @param aBottle The bottle to be filled.
 @param theData The value to be sent.
 @param topLevel @c true if this is the outermost list of an object. */
static void fillBottleFromValue(JSContext *        jct,
                                yarp::os::Bottle & aBottle,
                                JS::Value          theData,
                                const bool         topLevel)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "aBottle = ", &aBottle); //####
    OD_LOG_B1("topLevel = ", topLevel); //####
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
        
        aBottle.addString(YarpString(asChars));
        JS_free(jct, asChars);
    }
    else if (theData.isObject())
    {
        JS::RootedObject asObject(jct);
        
        if (JS_ValueToObject(jct, asRootedValue, &asObject))
        {
            bool processed = false;
            
            if (JS_IsArrayObject(jct, asObject))
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
                JS::AutoIdArray      ids(jct, JS_Enumerate(jct, asObject));
                yarp::os::Property & innerDict(aBottle.addDict());
                
                // Note that only operator! is defined, so we need to do a 'double-negative'.
                if (!! ids)
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
    OD_LOG_EXIT(); //####
} // fillBottleFromValue

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

CommonLispService::CommonLispService(const Utilities::DescriptorVector & argumentList,
                                     JSContext *                         context,
                                     JS::RootedObject &                  global,
                                     const YarpString &                  launchPath,
                                     const int                           argc,
                                     char * *                            argv,
                                     const YarpString &                  tag,
                                     const YarpString &                  description,
                                     const Common::ChannelVector &       loadedInletDescriptions,
                                     const Common::ChannelVector &       loadedOutletDescriptions,
                                     const JS::AutoValueVector &         loadedInletHandlers,
                                     const JS::RootedValue &             loadedStartingFunction,
                                     const JS::RootedValue &             loadedStoppingFunction,
                                     const bool                          sawThread,
                                     const JS::RootedValue &             loadedThreadFunction,
                                     const double                        loadedInterval,
                                     const YarpString &                  serviceEndpointName,
                                     const YarpString &                  servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true, MpM_JAVASCRIPT_CANONICAL_NAME_,
              description, "", serviceEndpointName, servicePortNumber), _inletHandlers(context),
    _inHandlers(), _generator(NULL), _context(context), _global(global),
    _loadedInletDescriptions(loadedInletDescriptions),
    _loadedOutletDescriptions(loadedOutletDescriptions), _scriptStartingFunc(context),
    _scriptStoppingFunc(context), _scriptThreadFunc(context), _threadInterval(loadedInterval),
    _isThreaded(sawThread)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P4("argumentList = ", &argumentList, "context = ", context, "global = ", &global, //####
              "argv = ", argv); //####
    OD_LOG_P4("loadedInletDescriptions = ", &loadedInletDescriptions, //####
              "loadedOutletDescriptions = ", &loadedOutletDescriptions, //####
              "loadedInletHandlers = ", &loadedInletHandlers, "loadedStartingFunction = ", //####
              &loadedStartingFunction); //####
    OD_LOG_P2("loadedStoppingFunction = ", &loadedStoppingFunction, //####
              "loadedThreadFunction = ", &loadedThreadFunction); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "description = ", description, //####
               "serviceEndpointName = ", serviceEndpointName); //####
    OD_LOG_S1s("servicePortNumber = ", servicePortNumber); //####
    OD_LOG_B1("sawThread = ", sawThread); //####
    OD_LOG_D1("loadedInterval = ", loadedInterval); //####
    JS_SetContextPrivate(context, this);
    _inletHandlers.appendAll(loadedInletHandlers);
    _scriptStartingFunc = loadedStartingFunction;
    _scriptStoppingFunc = loadedStoppingFunction;
    _scriptThreadFunc = loadedThreadFunction;
    OD_LOG_EXIT_P(this); //####
} // CommonLispService::CommonLispService

CommonLispService::~CommonLispService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    releaseHandlers();
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::~CommonLispService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
DEFINE_CONFIGURE_(CommonLispService)
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(details)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
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
                OD_LOG("! (JS_CallFunctionValue(_context, _global, _scriptStartingFunc, " //####
                       "funcArgs, &funcResult))"); //####
                JS::RootedValue exc(_context);
                
                if (JS_GetPendingException(_context, &exc))
                {
                    JS_ClearPendingException(_context);
#if MAC_OR_LINUX_
                    GetLogger().fail("Exception occurred while executing scriptStarting function.");
#else // ! MAC_OR_LINUX_
                    cerr << "Exception occurred while executing scriptStarting function." << endl;
#endif // ! MAC_OR_LINUX_
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
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // CommonLispService::configure
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

DEFINE_GETCONFIGURATION_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // CommonLispService::getConfiguration

void CommonLispService::releaseHandlers(void)
{
    OD_LOG_OBJENTER(); //####
    if (0 < _inHandlers.size())
    {
        for (HandlerVector::const_iterator walker(_inHandlers.begin()); _inHandlers.end() != walker;
             ++walker)
        {
            CommonLispInputHandler * aHandler = *walker;
            
            if (aHandler)
            {
                OD_LOG_P1("aHandler = ", aHandler); //####
                delete aHandler;
            }
        }
        _inHandlers.clear();
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::releaseHandlers

DEFINE_RESTARTSTREAMS_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        // No special processing needed.
        stopStreams();
        startStreams();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::restartStreams

bool CommonLispService::sendToChannel(const int32_t channelSlot,
                                      JS::Value     theData)
{
    OD_LOG_OBJENTER();
    OD_LOG_L1("channelSlot = ", channelSlot); //####
    bool okSoFar = false;
    
    if ((0 <= channelSlot) && (channelSlot < static_cast<int32_t>(getOutletCount())))
    {
        Common::GeneralChannel * outChannel = getOutletStream(channelSlot);
        yarp::os::Bottle         outBottle;
        
        fillBottleFromValue(_context, outBottle, theData, true);
        if ((0 < outBottle.size()) && outChannel)
        {
            if (outChannel->write(outBottle))
            {
                okSoFar = true;
            }
            else
            {
                OD_LOG("! (outChannel->write(message))"); //####
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
    OD_LOG_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // CommonLispService::sendToChannel

DEFINE_SETUPSTREAMDESCRIPTIONS_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
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
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // CommonLispService::setUpStreamDescriptions

DEFINE_STARTSERVICE_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isStarted())
        {
            inherited::startService();
            if (isStarted())
            {
                
            }
            else
            {
                OD_LOG("! (isStarted())"); //####
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(isStarted()); //####
    return isStarted();
} // CommonLispService::startService

DEFINE_STARTSTREAMS_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_isThreaded)
            {
                _generator = new CommonLispThread(_threadInterval, _context, _global,
                                                  _scriptThreadFunc);
				if (! _generator->start())
				{
					OD_LOG("(! _generator->start())"); //####
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
                    JS::HandleValue          handlerFunc = _inletHandlers[ii];
                    CommonLispInputHandler * aHandler = new CommonLispInputHandler(this, ii,
                                                                                   handlerFunc);
                    
                    if (aHandler)
                    {
                        _inHandlers.push_back(aHandler);
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
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::startStreams

DEFINE_STOPSERVICE_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    bool result;
    
    try
    {
        result = inherited::stopService();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // CommonLispService::stopService

DEFINE_STOPSTREAMS_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
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
						yarp::os::Time::delay(_threadInterval / 3.9);
					}
					delete _generator;
					_generator = NULL;
				}
            }
            else
            {
                for (size_t ii = 0, mm = getInletCount(); mm > ii; ++ii)
                {
                    CommonLispInputHandler * aHandler = _inHandlers.at(ii);
                    
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
                    OD_LOG("! (JS_CallFunctionValue(_context, _global, _scriptStoppingFunc, " //####
                           "funcArgs, &funcResult))"); //####
                    JS::RootedValue exc(_context);
                    
                    if (JS_GetPendingException(_context, &exc))
                    {
                        JS_ClearPendingException(_context);
#if MAC_OR_LINUX_
                        GetLogger().fail("Exception occurred while executing scriptStopping "
                                         "function.");
#else // ! MAC_OR_LINUX_
                        cerr << "Exception occurred while executing scriptStopping function." <<
                                endl;
#endif // ! MAC_OR_LINUX_
                    }
                }
                JS_EndRequest(_context);
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

std::ostream & CommonLisp::PrintCommonLispObject(std::ostream &     outStream,
                                                 JSContext *        jct,
                                                 JS::RootedObject & anObject,
                                                 const int          depth)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "anObject = ", &anObject); //####
    OD_LOG_L1("depth = ", depth); //####
    JS::AutoIdArray ids(jct, JS_Enumerate(jct, anObject));
    
    // Note that only operator! is defined, so we need to do a 'double-negative'.
    if (!! ids)
    {
        bool okSoFar = true;
        
        for (size_t ii = 0, len = ids.length(); (len > ii) && okSoFar; ++ii)
        {
            JS::RootedValue key(jct);
            
            if (JS_IdToValue(jct, ids[ii], &key))
            {
                PrintCommonLispValue(outStream, jct, "id = ", key, depth);
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
                    PrintCommonLispValue(outStream, jct, ", property = ", result, 0);
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
                            if (JS_IsArrayObject(jct, result))
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
                                    outStream << ", arity = " << JS_GetFunctionArity(asFunction);
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
                            PrintCommonLispObject(outStream, jct, asObject, depth + 1);
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
    OD_LOG_EXIT_P(&outStream); //####
    return outStream;
} // CommonLisp::PrintCommonLispObject

std::ostream & CommonLisp::PrintCommonLispValue(std::ostream &    outStream,
                                                JSContext *       jct,
                                                const char *      caption,
                                                JS::RootedValue & value,
                                                const int         depth)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("outStream = ", &outStream, "jct = ", jct, "value = ", &value); //####
    OD_LOG_S1("caption = ", caption); //####
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
    OD_LOG_EXIT_P(&outStream); //####
    return outStream;
} // CommonLisp::PrintCommonLispValue
#endif//0
