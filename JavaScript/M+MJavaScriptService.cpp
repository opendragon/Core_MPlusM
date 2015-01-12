//--------------------------------------------------------------------------------------------------
//
//  File:       M+MJavaScriptService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the JavaScript input / output service.
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

#include "M+MJavaScriptService.h"
#include "M+MJavaScriptInputHandler.h"
#include "M+MJavaScriptRequests.h"

#include <mpm/M+MEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the %JavaScript input / output service. */
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

/*! @brief Fill a bottle with the contents of an object.
 @param jct The %JavaScript engine context.
 @param aBottle The bottle to be filled.
 @param theData The value to be sent. */
static void fillBottleFromValue(JSContext *        jct,
                                yarp::os::Bottle & aBottle,
                                JS::Value          theData)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "aBottle = ", &aBottle); //####
                    //TBD --> copy values from theData to aBottle
#if 0
    JS::RootedObject asObject(jct);
    
    if (JS_ValueToObject(jct, theData, &asObject))
    {
        PrintJavaScriptObject(std::cout, jct, asObject, 0);
    }
#endif//0
    yarp::os::Property & aProp = aBottle.addDict();

    aProp.put("1", "12");
    aProp.put("2", "24");
    OD_LOG_EXIT(); //####
} // fillBottleFromValue

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

JavaScriptService::JavaScriptService(JSContext *                   context,
                                     JS::RootedObject &            global,
                                     const yarp::os::ConstString & launchPath,
                                     const yarp::os::ConstString & tag,
                                     const yarp::os::ConstString & description,
                                     const Common::ChannelVector & loadedInletDescriptions,
                                     const Common::ChannelVector & loadedOutletDescriptions,
                                     const JS::AutoValueVector &   loadedInletHandlers,
                                     const yarp::os::ConstString & serviceEndpointName,
                                     const yarp::os::ConstString & servicePortNumber) :
    inherited(launchPath, tag, true, MpM_JAVASCRIPT_CANONICAL_NAME, description, "",
              serviceEndpointName, servicePortNumber), _inletHandlers(context), _inHandlers(),
    _context(context), _global(global), _loadedInletDescriptions(loadedInletDescriptions),
    _loadedOutletDescriptions(loadedOutletDescriptions)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P4("context = ", context, "global = ", &global, "loadedInletDescriptions = ", //####
              &loadedInletDescriptions, "loadedOutletDescriptions = ", //####
              &loadedOutletDescriptions); //####
    OD_LOG_P1("loadedInletHandlers = ", &loadedInletHandlers); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "description = ", description, //####
               "serviceEndpointName = ", serviceEndpointName); //####
    OD_LOG_S1s("servicePortNumber = ", servicePortNumber); //####
    JS_SetContextPrivate(context, this);
    _inletHandlers.appendAll(loadedInletHandlers);
    OD_LOG_EXIT_P(this); //####
} // JavaScriptService::JavaScriptService

JavaScriptService::~JavaScriptService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    releaseHandlers();
    OD_LOG_OBJEXIT(); //####
} // JavaScriptService::~JavaScriptService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool JavaScriptService::configure(const yarp::os::Bottle & details)
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
        // Nothing needs to be done.
        result = true;
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(); //####
    return result;
} // JavaScriptService::configure
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void JavaScriptService::releaseHandlers(void)
{
    OD_LOG_OBJENTER(); //####
    if (0 < _inHandlers.size())
    {
        for (HandlerVector::const_iterator walker(_inHandlers.begin()); _inHandlers.end() != walker;
             ++walker)
        {
            JavaScriptInputHandler * aHandler = *walker;
            
            if (aHandler)
            {
                OD_LOG_P1("aHandler = ", aHandler); //####
                delete aHandler;
            }
        }
        _inHandlers.clear();
    }
    OD_LOG_OBJEXIT(); //####
} // JavaScriptService::releaseHandlers

void JavaScriptService::restartStreams(void)
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
} // JavaScriptService::restartStreams

bool JavaScriptService::sendToChannel(const int32_t channelSlot,
                                      JS::Value     theData)
{
    OD_LOG_OBJENTER();
    OD_LOG_L1("channelSlot = ", channelSlot); //####
    bool okSoFar = false;
    
    if ((0 <= channelSlot) && (channelSlot < getOutletCount()))
    {
        Common::GeneralChannel * outChannel = _outStreams.at(channelSlot);
        yarp::os::Bottle         outBottle;
        
        fillBottleFromValue(_context, outBottle, theData);
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
} // JavaScriptService::sendToChannel

bool JavaScriptService::setUpStreamDescriptions(void)
{
    OD_LOG_OBJENTER(); //####
    bool                  result = true;
    ChannelDescription    description;
    yarp::os::ConstString rootName(getEndpoint().getName() + "/");
    
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
} // JavaScriptService::setUpStreamDescriptions

bool JavaScriptService::start(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isStarted())
        {
            inherited::start();
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
} // JavaScriptService::start

void JavaScriptService::startStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            releaseHandlers();
            for (size_t ii = 0, mm = _inStreams.size(); mm > ii; ++ii)
            {
                //TBD we need a reference to the JavaScript handler here
                JS::HandleValue          handlerFunc = _inletHandlers[ii];
                JavaScriptInputHandler * aHandler = new JavaScriptInputHandler(this, ii,
                                                                               handlerFunc);
                
                if (aHandler)
                {
                    _inHandlers.push_back(aHandler);
                    _inStreams.at(ii)->setReader(*aHandler);
                    aHandler->activate();
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
} // JavaScriptService::startStreams

bool JavaScriptService::stop(void)
{
    OD_LOG_OBJENTER(); //####
    bool result;
    
    try
    {
        result = inherited::stop();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // JavaScriptService::stop

void JavaScriptService::stopStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            for (size_t ii = 0, mm = _inStreams.size(); mm > ii; ++ii)
            {
                JavaScriptInputHandler * aHandler = _inHandlers.at(ii);
                
                if (aHandler)
                {
                    aHandler->deactivate();
                }
            }
            clearActive();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // JavaScriptService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

void JavaScript::PrintJavaScriptObject(std::ostream &     outStream,
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
        
        for (int ii = 0, len = ids.length(); (len > ii) && okSoFar; ++ii)
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
                        outStream << std::endl;
                        if (okSoFar)
                        {
                            PrintJavaScriptObject(outStream, jct, asObject, depth + 1);
                        }
                    }
                    else
                    {
                        outStream << std::endl;
                    }
                }
            }
        }
    }
    OD_LOG_EXIT(); //####
} // JavaScript::PrintJavaScriptObject

void JavaScript::PrintJavaScriptValue(std::ostream &    outStream,
                                      JSContext *       jct,
                                      const char *      caption,
                                      JS::RootedValue & value,
                                      const int         depth)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("outStream = ", &outStream, "jct = ", jct, "value = ", value); //####
    OD_LOG_S1("caption = ", caption); //####
    if (0 < depth)
    {
        outStream.width(depth);
        outStream << " ";
    }
    std::cout << caption;
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
    OD_LOG_EXIT(); //####
} // JavaScript::PrintJavaScriptValue
